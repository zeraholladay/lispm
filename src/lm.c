#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

#include "dict.h"
#include "keywords.h"
#include "lm.h"
#include "lm_env.h"
#include "lm_err.h"
#include "lm_gc.h"
#include "palloc.h"
#include "prelude.h"
#include "prims.h"
#include "syntax.h"
#include "thunks.h"
#include "types.h"
#include "xalloc.h"

#define S(tag, ...)                                                           \
  (State)                                                                     \
  {                                                                           \
    .s = s_##tag, .u.tag = { __VA_ARGS__ }                                    \
  }

static inline Cell *
stk_pop (LM *lm)
{
  if (lm->stk.sp == 0)
    return lm_err_null (lm, ERR_UNDERFLOW, "stk_pop");

  return lm->stk.cells[--lm->stk.sp];
}

static inline bool
stk_push (LM *lm, Cell *val)
{
  if (lm->stk.sp >= LISPM_STK_MAX)
    return lm_err_bool (lm, ERR_OVERFLOW, "stk_push");

  lm->stk.cells[lm->stk.sp++] = val;
  return true;
}

static inline bool
ctl_pop (LM *lm, State *out)
{
  if (lm->ctl.sp == 0)
    return lm_err_bool (lm, ERR_UNDERFLOW, "ctl_pop");

  *out = lm->ctl.states[--lm->ctl.sp];
  return true;
}

static inline bool
ctl_push_state (LM *lm, State st)
{
  if (lm->ctl.sp >= LISPM_CTL_MAX)
    return lm_err_null (lm, ERR_OVERFLOW, "ctl_push_state");

  lm->ctl.states[lm->ctl.sp++] = st;
  // Record peak depth, not just the empty stack seen after a call returns.
  // Tests compare this value for short and long tail-recursive runs.
  if (lm->ctl.sp > lm->ctl.high_water)
    lm->ctl.high_water = lm->ctl.sp;
  return true;
}

static inline bool
ctl_push_states (LM *lm, const State sts[], size_t n)
{
  // The control stack is LIFO: push in reverse so sts[0] executes first.
  while (n-- > 0)
    {
      if (!ctl_push_state (lm, sts[n]))
        return false;
    }
  return true;
}

static Cell *
lambda_capture (LM *lm, Cell *lambda)
{
  Cell *closure = LAMBDA (lambda->lambda.params, lambda->lambda.body, lm);
  closure->lambda.env = lm->env.current;
  return closure;
}

static void
lm_reset (LM *lm)
{
  fprintf (stderr,
           "Registers:\n"
           "  stk: %zu\n"
           "  env: %p\n"
           "  ctl: %zu\n",
           lm->stk.sp, (void *)lm->env.current, lm->ctl.sp);

  // Reset stack pointers and restore the permanent global frame.
  lm->stk.sp      = 0;
  lm->env.current = lm->env.global;
  lm->ctl.sp      = 0;

  lm->err_bool = false;
}

static inline bool
lm_eval_switch (LM *lm, StateEnum s, Union u)
{
  switch (s)
    {
#define X(tag, ...)                                                           \
  case s_##tag:                                                               \
    goto ctl_##tag;
#include "lm.def"
#undef X
    default:
      return lm_err_bool (lm, ERR_INTERNAL, "no such state");
    }

ctl_closure_enter:
  {
    // Tail calls reuse a return continuation, not the caller's bindings.
    // A fresh frame lets closures keep observing the old frame unchanged.
    assert (IS_INST (u.closure_enter.fn->lambda.env, ENV));
    lm->env.current = ENV (u.closure_enter.fn->lambda.env, lm);
    return true;
  }

ctl_closure_leave:
  {
    // This is a return continuation: restore the environment of the code
    // that will resume next. GC owns the departed frame's lifetime.
    lm->env.current = u.closure_leave.env;
    return true;
  }

ctl_eval:
  {
    Cell *expr = u.eval.expr;

    if (!expr && !(expr = stk_pop (lm)))
      return false;

    // Lookup symbol.
    if (IS_INST (expr, SYMBOL))
      return stk_push (lm, lm_env_lookup (lm, expr));

    // Named definitions contain a bare lambda AST; anonymous lambdas are
    // wrapped.
    if (IS_INST (expr, LAMBDA) && NILP (expr->lambda.env))
      return stk_push (lm, lambda_capture (lm, expr));

    // Literals
    if (!LISTP (expr))
      return stk_push (lm, expr);

    if (LISTP (expr))
      {
        // nil
        if (NILP (expr))
          return stk_push (lm, NIL);

        if (!proper_list_p (expr))
          return lm_err_bool (lm, ERR_INVALID_ARG, "eval: improper call form");

        Cell *car = CAR (expr);
        Cell *cdr = CDR (expr);

        // quote is a special symbol.
        if (car == QUOTE)
          {
            if (length (cdr) != 1)
              return lm_err_bool (lm, ERR_INVALID_ARG_LEN,
                                  "quote: expected one argument");
            return stk_push (lm, CAR (cdr));
          }

        // Lambda is an AST-type.
        if (IS_INST (car, LAMBDA))
          return stk_push (lm, lambda_capture (lm, car));

        return ctl_push_state (lm, S (eval_apply, .fn = car, .arglist = cdr));
      }

    return lm_err_bool (lm, ERR_INTERNAL, "eval");
  }

ctl_eval_apply:
  {
    Cell *fn      = u.eval_apply.fn;
    Cell *arglist = u.eval_apply.arglist;

    // If fn is not NULL, then evalute fn.
    if (fn)
      return ctl_push_states (
          lm,
          (State[]){
              S (eval, .expr = fn),
              S (eval_apply, .fn = NULL, .arglist = arglist),
          },
          2);

    // fn was NULL, so pop the evaluated value from the stack.
    if (!(fn = stk_pop (lm)))
      return false;

    // One of the special fns this C code handles
    if (IS_INST (fn, THUNK) && thunk_sf_bool (fn))
      return ctl_push_state (lm, S (lispm, .fn = fn, .arglist = arglist));

    return ctl_push_states (lm,
                            (State[]){
                                S (evlis, .acc = NIL, .arglist = arglist),
                                S (funcall, .fn = fn, .arglist = NULL),
                            },
                            2);
  }

// list
ctl_evlis:
  {
    Cell *acc     = u.evlis.acc;
    Cell *arglist = u.evlis.arglist;

    // Done.
    if (NILP (arglist))
      {
        Cell *res = reverse_inplace (acc);
        return stk_push (lm, res);
      }

    Cell *car = CAR (arglist);
    Cell *cdr = CDR (arglist);

    return ctl_push_states (lm,
                            (State[]){
                                S (eval, .expr = car, ),
                                S (evlis_acc, .acc = acc, .arglist = cdr),
                            },
                            2);
  }

ctl_evlis_acc:
  {
    Cell *acc     = u.evlis_acc.acc;
    Cell *arglist = u.evlis_acc.arglist;

    Cell *res = stk_pop (lm);
    if (!res)
      return false;

    Cell *acc2 = CONS (res, acc, lm);
    return ctl_push_state (lm, S (evlis, .acc = acc2, .arglist = arglist));
  }

ctl_funcall:
  {
    Cell *fn      = u.funcall.fn;
    Cell *arglist = u.funcall.arglist;

    if (!fn && !(fn = stk_pop (lm)))
      return false;

    if (!arglist && !(arglist = stk_pop (lm)))
      return false;

    if (!proper_list_p (arglist))
      return lm_err_bool (lm, ERR_INVALID_ARG,
                          "funcall: improper argument list");

    if (IS_INST (fn, THUNK))
      return stk_push (lm, thunker (lm, fn, arglist));

    if (IS_INST (fn, LAMBDA))
      {
        // Tail position is encoded by the next continuation. If it is a
        // closure_leave, the current function has no work left after this
        // call. Any older caller's work remains BELOW that return state.
        //
        // For f tail-calling g, execution order is:
        //   enter g -> run g -> existing return to f's caller
        // rather than:
        //   enter g -> run g -> return to f -> return to f's caller
        //
        // Calls in arguments, predicates, or non-final body expressions
        // have other continuation states on top, so they keep their return.
        bool tail = lm->ctl.sp > 0
                    && lm->ctl.states[lm->ctl.sp - 1].s == s_closure_leave;
        if (!tail
            && !ctl_push_state (lm, S (closure_leave, .env = lm->env.current)))
          return false;

        // Leave the existing return in place for a tail call. Both cases
        // enter a fresh frame, then bind arguments and evaluate the body.
        return ctl_push_states (
            lm,
            (State[]){ S (closure_enter, .fn = fn),
                       S (lambda, .fn = fn, .arglist = arglist) },
            2);
      }

    return lm_err_bool (lm, ERR_NOT_A_FN, "funcall");
  }

ctl_lambda:
  {
    Cell *fn      = u.lambda.fn;
    Cell *arglist = u.lambda.arglist;

    if (!fn && !(fn = stk_pop (lm)))
      return false;

    if (!arglist && !(arglist = stk_pop (lm)))
      return false;

    Lambda *lambda = &fn->lambda;
    Cell   *params = lambda->params;

    // Each cons in the formal specification names a required argument.
    // Walk the argument list directly instead of requiring equal lengths or
    // allocating a zipped list of bindings. funcall already checked its shape.
    while (CONSP (params))
      {
        if (NILP (arglist))
          return lm_err_bool (lm, ERR_MISSING_ARG, "lambda");
        if (!lm_env_define (lm, CAR (params), CAR (arglist)))
          return false;
        params  = CDR (params);
        arglist = CDR (arglist);
      }

    // A final symbol captures all remaining values; for a rest-only lambda
    // it captures the whole list. Binding NIL is valid when no values remain.
    // The suffix becomes an environment root, so escaped closures retain it.
    if (IS_INST (params, SYMBOL))
      {
        if (!lm_env_define (lm, params, arglist))
          return false;
      }
    else if (!NILP (params))
      return lm_err_bool (lm, ERR_INVALID_ARG, "lambda: invalid parameters");
    else if (!NILP (arglist))
      return lm_err_bool (lm, ERR_UNEXPECTED_ARG, "lambda");

    return ctl_push_state (lm, S (progn, .res = NIL, .arglist = lambda->body));
  }

ctl_define:
  {
    Cell *sym = u.define.sym;
    Cell *cdr = stk_pop (lm);

    if (!cdr)
      return false;

    if (!CONSP (cdr) || !NILP (CDR (cdr)))
      return lm_err_bool (lm, ERR_UNEXPECTED_ARG, "define");

    if (!lm_env_define (lm, sym, CAR (cdr)))
      return false;

    return stk_push (lm, CAR (cdr));
  }

ctl_set:
  {
    Cell *sym = u.set.sym;
    Cell *cdr = stk_pop (lm);

    if (!cdr)
      return false;

    if (!CONSP (cdr) || !NILP (CDR (cdr)))
      return lm_err_bool (lm, ERR_UNEXPECTED_ARG, "set!");

    if (!lm_env_set (lm, sym, CAR (cdr)))
      return false;

    return stk_push (lm, CAR (cdr));
  }

ctl_apply:
  {
    Cell *fn      = u.apply.fn;
    Cell *arglist = u.apply.arglist;

    if (!fn && !(fn = stk_pop (lm)))
      return false;

    if (!arglist && !(arglist = stk_pop (lm)))
      return false;

    if (!proper_list_p (arglist) || NILP (arglist))
      return lm_err_bool (lm, ERR_MISSING_ARG, "apply: not a list.");

    // Copy fixed arguments in reverse order, then link the fresh cells onto
    // the final list. Never reverse or overwrite any caller-owned list spine.
    Cell *fixed = NIL;
    while (!NILP (CDR (arglist)))
      {
        fixed   = CONS (CAR (arglist), fixed, lm);
        arglist = CDR (arglist);
      }
    Cell *all = CAR (arglist);
    if (!proper_list_p (all))
      return lm_err_bool (lm, ERR_MISSING_ARG, "apply: last not a list.");
    while (CONSP (fixed))
      {
        Cell *next = CDR (fixed);
        RPLACD (fixed, all);
        all   = fixed;
        fixed = next;
      }

    return ctl_push_state (lm, S (funcall, .fn = fn, .arglist = all));
  }

ctl_progn:
  {
    Cell *res     = u.progn.res;
    Cell *arglist = u.progn.arglist;

    if (NILP (arglist))
      return stk_push (lm, res);

    // Earlier expressions need progn_eval to discard their result and
    // continue. The last expression's result IS the sequence's result, so
    // evaluate it without leaving a sequencing continuation on the stack.
    // This exposes the enclosing return state to any call in that expression.
    if (NILP (CDR (arglist)))
      return ctl_push_state (lm, S (eval, .expr = CAR (arglist)));

    return ctl_push_states (
        lm,
        (State[]){ S (eval, .expr = CAR (arglist)),
                   S (progn_eval, .arglist = CDR (arglist)) },
        2);
  }

ctl_progn_eval:
  {
    Cell *res     = stk_pop (lm);
    Cell *arglist = u.progn_eval.arglist;

    if (!res)
      return false;

    return ctl_push_state (lm, S (progn, .res = res, .arglist = arglist));
  }

ctl_lispm:
  {
    Cell *fn      = u.lispm.fn;
    Cell *arglist = u.lispm.arglist;

    if (!proper_list_p (arglist))
      return lm_err_bool (lm, ERR_INVALID_ARG,
                          "special form: improper arguments");
    size_t received = length (arglist);
    size_t minimum  = 0;
    size_t maximum  = (size_t)-1;
    switch (fn->thunk)
      {
      case THUNK_DEFINE:
      case THUNK_SET:
        minimum = maximum = 2;
        break;
      case THUNK_EVAL:
        minimum = maximum = 1;
        break;
      case THUNK_IF:
        minimum = 2;
        maximum = 3;
        break;
      case THUNK_LET:
      case THUNK_APPLY:
        minimum = 2;
        break;
      case THUNK_FUNCALL:
        minimum = 1;
        break;
      default:
        break;
      }
    if (received < minimum || received > maximum)
      return lm_err_bool (
          lm, received < minimum ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG, "%s",
          thunk_get_name (fn));

    switch (fn->thunk)
      {
      case THUNK_APPLY:
        return ctl_push_states (
            lm,
            (State[]){
                S (evlis, .acc = NIL, .arglist = CDR (arglist)),
                S (eval, .expr = CAR (arglist)),
                S (apply, .fn = NULL, .arglist = NULL),
            },
            3);

      case THUNK_FUNCALL:
        return ctl_push_states (
            lm,
            (State[]){
                S (evlis, .acc = NIL, .arglist = CDR (arglist)),
                S (eval, .expr = CAR (arglist)),
                S (funcall, .fn = NULL, .arglist = NULL),
            },
            3);

      case THUNK_DEFINE:
        if (!CONSP (arglist) || !CONSP (CDR (arglist))
            || !NILP (CDDR (arglist)))
          return lm_err_bool (lm, ERR_UNEXPECTED_ARG, "define");
        return ctl_push_states (
            lm,
            (State[]){
                S (evlis, .acc = NIL, .arglist = CDR (arglist)),
                S (define, .sym = CAR (arglist)),
            },
            2);

      case THUNK_SET:
        if (!CONSP (arglist) || !CONSP (CDR (arglist))
            || !NILP (CDDR (arglist)))
          return lm_err_bool (lm, ERR_UNEXPECTED_ARG, "set!");
        return ctl_push_states (
            lm,
            (State[]){
                S (evlis, .acc = NIL, .arglist = CDR (arglist)),
                S (set, .sym = CAR (arglist)),
            },
            2);

      case THUNK_EVAL:
        return ctl_push_states (lm,
                                (State[]){
                                    S (eval, .expr = CAR (arglist)),
                                    S (eval, .expr = NULL),
                                },
                                2);

      case THUNK_PROGN:
        return ctl_push_state (lm, S (progn, .res = NIL, .arglist = arglist));

      case THUNK_IF:
        return ctl_push_state (lm, S (if_, .form = arglist));

      case THUNK_AND:
      case THUNK_OR:
      case THUNK_LET:
        {
          Cell *lowered = syntax_lower (lm, fn->thunk, arglist);
          return lowered && ctl_push_state (lm, S (eval, .expr = lowered));
        }

      default:
        break;
      }

    return lm_err_bool (lm, ERR_INTERNAL, "lispm");
  }

ctl_if_:
  {
    Cell *form = u.if_.form;

    return ctl_push_states (lm,
                            (State[]){
                                S (eval, .expr = CAR (form)),
                                S (if_cont, .form = CDR (form)),
                            },
                            2);
  }

ctl_if_cont:
  {
    Cell *form = u.if_cont.form;
    Cell *pred = stk_pop (lm);

    if (!pred)
      return false;

    if (!NILP (pred))
      {
        // Lowered or returns this already evaluated predicate without a
        // temporary lexical binding or a second evaluation of the operand.
        if (CAR (form) == syntax_predicate_value)
          return stk_push (lm, pred);
        return ctl_push_state (lm, S (eval, .expr = CAR (form)));
      }

    Cell *else_form = CAR (CDR (form));

    if (else_form)
      return ctl_push_state (lm, S (eval, .expr = else_form));
    else
      return stk_push (lm, NIL);
  }
}

static Cell *
lm_eval (LM *lm)
{
  size_t steps    = 0;
  size_t base_ptr = lm->ctl.sp;
  State  state;

  do
    {
      // A primitive can push NIL and set err_bool while its VM step itself
      // succeeds. Check again AFTER the step: a tail-position primitive may
      // be the final step, with no later continuation to notice the error.
      if (lm->err_bool || !ctl_pop (lm, &state)
          || !lm_eval_switch (lm, state.s, state.u) || lm->err_bool)
        {
          lm_reset (lm);
          return NIL;
        }

      // Collect only between VM steps, after temporary values have been
      // rooted.
      if (++steps >= 256)
        {
          lm_gc (lm);
          steps = 0;
        }
    }
  while (base_ptr <= lm->ctl.sp);

  return stk_pop (lm);
}

LM *
lm_create (void)
{
  // Initialize NIL
  CAR (NIL) = CDR (NIL) = NIL;

  // Zeros out stack pointers
  LM *lm = xcalloc (1, sizeof *(lm));

  // Allocate pool
  lm->pool       = pool_init (LM_CAP_PER_POOL, sizeof (Cell));
  lm->env.global = lm->env.current = ENV (NULL, lm);

  if (!prelude_load (lm))
    {
      fputs ("Failed to load the Scheme prelude\n", stderr);
      lm_destroy (lm);
      return NULL;
    }

  return lm;
}

static void
dispose_allocated_cell (Pool *pool, void *ptr)
{
  (void)pool;
  if (!pool_gc_is_free (ptr))
    cell_dispose (ptr);
}

void
lm_destroy (LM *lm)
{
  pool_map_hier (lm->pool, dispose_allocated_cell);
  pool_destroy_hier (&lm->pool);
  free (lm);
}

Cell *
lm_alloc_cell (LM *lm)
{
  return pool_xalloc_hier (&lm->pool);
}

Cell *
lm_progn (LM *lm, Cell *progn)
{
  // Measure each evaluation separately, including its initial progn state.
  lm->ctl.high_water = lm->ctl.sp;
  if (!ctl_push_state (lm, S (progn, .res = NIL, .arglist = progn)))
    {
      lm_reset (lm);
      return NIL;
    }

  return lm_eval (lm);
}
