#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

#include "builtins.h"
#include "dict.h"
#include "keywords.h"
#include "lm.h"
#include "lm_env.h"
#include "lm_err.h"
#include "lm_gc.h"
#include "lm_states.h"
#include "palloc.h"
#include "prelude.h"
#include "prims.h"
#include "syntax.h"
#include "types.h"
#include "xalloc.h"

static inline Cell *
stk_pop (LM *lm)
{
  if (lm->stk.sp == 0)
    return lm_fail_pointer (lm, ERR_UNDERFLOW, "stk_pop");

  return lm->stk.cells[--lm->stk.sp];
}

static inline bool
stk_push (LM *lm, Cell *val)
{
  if (lm->stk.sp >= LISPM_STK_MAX)
    return lm_fail (lm, ERR_OVERFLOW, "stk_push");

  lm->stk.cells[lm->stk.sp++] = val;
  return true;
}

static inline bool
ctl_pop (LM *lm, State *out)
{
  if (lm->ctl.sp == 0)
    return lm_fail (lm, ERR_UNDERFLOW, "ctl_pop");

  *out = lm->ctl.states[--lm->ctl.sp];
  return true;
}

static inline bool
ctl_push_state (LM *lm, State st)
{
  if (lm->ctl.sp >= LISPM_CTL_MAX)
    return lm_fail (lm, ERR_OVERFLOW, "ctl_push_state");

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

// Accept instructions in execution order; ctl_push_states pushes them in
// reverse for the LIFO control stack. Infer the count from the initializer.
// The repeated initializer is inside sizeof, so constructor arguments are
// evaluated only once; this array's inferred length is not variable-length.
#define PUSH_SEQ(lm, ...)                                                \
  ctl_push_states ((lm), (State[]){ __VA_ARGS__ },                            \
                   sizeof ((State[]){ __VA_ARGS__ }) / sizeof (State))

static Cell *
lambda_capture (LM *lm, Cell *lambda)
{
  Cell *closure
      = cell_make_lambda (lm, lambda->lambda.params, lambda->lambda.body);
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

  lm->has_error = false;
}

static inline bool
lm_eval_switch (LM *lm, StateKind kind, StatePayload payload)
{
  switch (kind)
    {
#define X(id, tag, ...)                                                       \
  case STATE_##id:                                                            \
    goto ctl_##tag;
#include "lm.def"
#undef X
    default:
      return lm_fail (lm, ERR_INTERNAL, "no such state");
    }

ctl_closure_enter:
  {
    // Tail calls reuse a return continuation, not the caller's bindings.
    // A fresh frame lets closures keep observing the old frame unchanged.
    assert (
        cell_has_type (payload.closure_enter.fn->lambda.env, CELL_TYPE_ENV));
    lm->env.current
        = cell_make_environment (lm, payload.closure_enter.fn->lambda.env);
    return true;
  }

ctl_closure_leave:
  {
    // This is a return continuation: restore the environment of the code
    // that will resume next. GC owns the departed frame's lifetime.
    lm->env.current = payload.closure_leave.env;
    return true;
  }

ctl_eval:
  {
    Cell *expr = payload.eval.expr;

    if (!expr && !(expr = stk_pop (lm)))
      return false;

    // Lookup symbol.
    if (cell_has_type (expr, CELL_TYPE_SYMBOL))
      return stk_push (lm, lm_env_lookup (lm, expr));

    // Named definitions contain a bare lambda AST; anonymous lambdas are
    // wrapped.
    if (cell_has_type (expr, CELL_TYPE_LAMBDA)
        && cell_is_nil (expr->lambda.env))
      return stk_push (lm, lambda_capture (lm, expr));

    // Literals
    if (!cell_is_pair_or_nil (expr))
      return stk_push (lm, expr);

    if (cell_is_pair_or_nil (expr))
      {
        // nil
        if (cell_is_nil (expr))
          return stk_push (lm, NIL);

        if (!cell_is_proper_list (expr))
          return lm_fail (lm, ERR_INVALID_ARG, "eval: improper call form");

        Cell *car = CAR (expr);
        Cell *cdr = CDR (expr);

        // quote is a special symbol.
        if (car == QUOTE)
          {
            if (length (cdr) != 1)
              return lm_fail (lm, ERR_INVALID_ARG_LEN,
                              "quote: expected one argument");
            return stk_push (lm, CAR (cdr));
          }

        // Lambda is an AST-type.
        if (cell_has_type (car, CELL_TYPE_LAMBDA))
          return stk_push (lm, lambda_capture (lm, car));

        return ctl_push_state (lm, eval_call (car, cdr));
      }

    return lm_fail (lm, ERR_INTERNAL, "eval");
  }

ctl_eval_apply:
  {
    Cell *fn      = payload.eval_apply.fn;
    Cell *arglist = payload.eval_apply.arglist;

    // If fn is not NULL, then evalute fn.
    if (fn)
      return PUSH_SEQ (lm, eval_expr (fn), continue_call (arglist));

    // fn was NULL, so pop the evaluated value from the stack.
    if (!(fn = stk_pop (lm)))
      return false;

    // One of the special fns this C code handles
    if (cell_has_type (fn, CELL_TYPE_BUILTIN) && builtin_is_special_form (fn))
      return ctl_push_state (lm, eval_special_form (fn, arglist));

    return PUSH_SEQ (lm, eval_args (arglist),
                          call_with_stacked_args (fn));
  }

// list
ctl_evlis:
  {
    Cell *acc     = payload.evlis.acc;
    Cell *arglist = payload.evlis.arglist;

    // Done.
    if (cell_is_nil (arglist))
      {
        Cell *res = reverse_inplace (acc);
        return stk_push (lm, res);
      }

    Cell *car = CAR (arglist);
    Cell *cdr = CDR (arglist);

    return PUSH_SEQ (lm, eval_expr (car), collect_arg (cdr, acc));
  }

ctl_evlis_acc:
  {
    Cell *acc     = payload.evlis_acc.acc;
    Cell *arglist = payload.evlis_acc.arglist;

    Cell *res = stk_pop (lm);
    if (!res)
      return false;

    Cell *acc2 = cell_make_pair (lm, res, acc);
    return ctl_push_state (lm, eval_remaining_args (arglist, acc2));
  }

ctl_funcall:
  {
    Cell *fn      = payload.funcall.fn;
    Cell *arglist = payload.funcall.arglist;

    if (!fn && !(fn = stk_pop (lm)))
      return false;

    if (!arglist && !(arglist = stk_pop (lm)))
      return false;

    if (!cell_is_proper_list (arglist))
      return lm_fail (lm, ERR_INVALID_ARG, "funcall: improper argument list");

    if (cell_has_type (fn, CELL_TYPE_BUILTIN))
      return stk_push (lm, dispatch_builtin (lm, fn, arglist));

    if (cell_has_type (fn, CELL_TYPE_LAMBDA))
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
        bool tail
            = lm->ctl.sp > 0
              && lm->ctl.states[lm->ctl.sp - 1].kind == STATE_CLOSURE_LEAVE;
        if (!tail && !ctl_push_state (lm, return_to (lm->env.current)))
          return false;

        // Leave the existing return in place for a tail call. Both cases
        // enter a fresh frame, then bind arguments and evaluate the body.
        return PUSH_SEQ (lm, enter_closure (fn),
                              run_lambda (fn, arglist));
      }

    return lm_fail (lm, ERR_NOT_A_FN, "funcall");
  }

ctl_lambda:
  {
    Cell *fn      = payload.lambda.fn;
    Cell *arglist = payload.lambda.arglist;

    if (!fn && !(fn = stk_pop (lm)))
      return false;

    if (!arglist && !(arglist = stk_pop (lm)))
      return false;

    Lambda *lambda = &fn->lambda;
    Cell   *params = lambda->params;

    // Each cons in the formal specification names a required argument.
    // Walk the argument list directly instead of requiring equal lengths or
    // allocating a zipped list of bindings. funcall already checked its shape.
    while (cell_is_pair (params))
      {
        if (cell_is_nil (arglist))
          return lm_fail (lm, ERR_MISSING_ARG, "lambda");
        if (!lm_env_define (lm, CAR (params), CAR (arglist)))
          return false;
        params  = CDR (params);
        arglist = CDR (arglist);
      }

    // A final symbol captures all remaining values; for a rest-only lambda
    // it captures the whole list. Binding NIL is valid when no values remain.
    // The suffix becomes an environment root, so escaped closures retain it.
    if (cell_has_type (params, CELL_TYPE_SYMBOL))
      {
        if (!lm_env_define (lm, params, arglist))
          return false;
      }
    else if (!cell_is_nil (params))
      return lm_fail (lm, ERR_INVALID_ARG, "lambda: invalid parameters");
    else if (!cell_is_nil (arglist))
      return lm_fail (lm, ERR_UNEXPECTED_ARG, "lambda");

    return ctl_push_state (lm, eval_body (lambda->body));
  }

ctl_define:
  {
    Cell *sym = payload.define.sym;
    Cell *cdr = stk_pop (lm);

    if (!cdr)
      return false;

    if (!cell_is_pair (cdr) || !cell_is_nil (CDR (cdr)))
      return lm_fail (lm, ERR_UNEXPECTED_ARG, "define");

    if (!lm_env_define (lm, sym, CAR (cdr)))
      return false;

    return stk_push (lm, CAR (cdr));
  }

ctl_set:
  {
    Cell *sym = payload.set.sym;
    Cell *cdr = stk_pop (lm);

    if (!cdr)
      return false;

    if (!cell_is_pair (cdr) || !cell_is_nil (CDR (cdr)))
      return lm_fail (lm, ERR_UNEXPECTED_ARG, "set!");

    if (!lm_env_set (lm, sym, CAR (cdr)))
      return false;

    return stk_push (lm, CAR (cdr));
  }

ctl_apply:
  {
    Cell *fn      = payload.apply.fn;
    Cell *arglist = payload.apply.arglist;

    if (!fn && !(fn = stk_pop (lm)))
      return false;

    if (!arglist && !(arglist = stk_pop (lm)))
      return false;

    if (!cell_is_proper_list (arglist) || cell_is_nil (arglist))
      return lm_fail (lm, ERR_MISSING_ARG, "apply: not a list.");

    // Copy fixed arguments in reverse order, then link the fresh cells onto
    // the final list. Never reverse or overwrite any caller-owned list spine.
    Cell *fixed = NIL;
    while (!cell_is_nil (CDR (arglist)))
      {
        fixed   = cell_make_pair (lm, CAR (arglist), fixed);
        arglist = CDR (arglist);
      }
    Cell *all = CAR (arglist);
    if (!cell_is_proper_list (all))
      return lm_fail (lm, ERR_MISSING_ARG, "apply: last not a list.");
    while (cell_is_pair (fixed))
      {
        Cell *next = CDR (fixed);
        cell_set_cdr (fixed, all);
        all   = fixed;
        fixed = next;
      }

    return ctl_push_state (lm, call_function (fn, all));
  }

ctl_progn:
  {
    Cell *res     = payload.progn.res;
    Cell *arglist = payload.progn.arglist;

    if (cell_is_nil (arglist))
      return stk_push (lm, res);

    // Earlier expressions need progn_eval to discard their result and
    // continue. The last expression's result IS the sequence's result, so
    // evaluate it without leaving a sequencing continuation on the stack.
    // This exposes the enclosing return state to any call in that expression.
    if (cell_is_nil (CDR (arglist)))
      return ctl_push_state (lm, eval_expr (CAR (arglist)));

    return PUSH_SEQ (lm, eval_expr (CAR (arglist)),
                          continue_body (CDR (arglist)));
  }

ctl_progn_eval:
  {
    Cell *res     = stk_pop (lm);
    Cell *arglist = payload.progn_eval.arglist;

    if (!res)
      return false;

    return ctl_push_state (lm, resume_body (arglist, res));
  }

ctl_special_form:
  {
    Cell *fn      = payload.special_form.fn;
    Cell *arglist = payload.special_form.arglist;

    if (!cell_is_proper_list (arglist))
      return lm_fail (lm, ERR_INVALID_ARG, "special form: improper arguments");
    size_t received = length (arglist);
    size_t minimum  = 0;
    size_t maximum  = (size_t)-1;
    switch (fn->builtin)
      {
      case BUILTIN_DEFINE:
      case BUILTIN_SET:
        minimum = maximum = 2;
        break;
      case BUILTIN_EVAL:
        minimum = maximum = 1;
        break;
      case BUILTIN_IF:
        minimum = 2;
        maximum = 3;
        break;
      case BUILTIN_LET:
      case BUILTIN_APPLY:
        minimum = 2;
        break;
      case BUILTIN_FUNCALL:
        minimum = 1;
        break;
      default:
        break;
      }
    if (received < minimum || received > maximum)
      return lm_fail (
          lm, received < minimum ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG, "%s",
          builtin_name (fn));

    switch (fn->builtin)
      {
      case BUILTIN_APPLY:
        return PUSH_SEQ (lm, eval_args (CDR (arglist)),
                              eval_expr (CAR (arglist)), apply_from_stack ());

      case BUILTIN_FUNCALL:
        return PUSH_SEQ (lm, eval_args (CDR (arglist)),
                              eval_expr (CAR (arglist)), call_from_stack ());

      case BUILTIN_DEFINE:
        if (!cell_is_pair (arglist) || !cell_is_pair (CDR (arglist))
            || !cell_is_nil (CDDR (arglist)))
          return lm_fail (lm, ERR_UNEXPECTED_ARG, "define");
        return PUSH_SEQ (lm, eval_args (CDR (arglist)),
                              define_from_stack (CAR (arglist)));

      case BUILTIN_SET:
        if (!cell_is_pair (arglist) || !cell_is_pair (CDR (arglist))
            || !cell_is_nil (CDDR (arglist)))
          return lm_fail (lm, ERR_UNEXPECTED_ARG, "set!");
        return PUSH_SEQ (lm, eval_args (CDR (arglist)),
                              set_from_stack (CAR (arglist)));

      case BUILTIN_EVAL:
        return PUSH_SEQ (lm, eval_expr (CAR (arglist)),
                              eval_from_stack ());

      case BUILTIN_PROGN:
        return ctl_push_state (lm, eval_body (arglist));

      case BUILTIN_IF:
        return ctl_push_state (lm, eval_if (arglist));

      case BUILTIN_AND:
      case BUILTIN_OR:
      case BUILTIN_LET:
        {
          Cell *lowered = syntax_lower (lm, fn->builtin, arglist);
          return lowered && ctl_push_state (lm, eval_expr (lowered));
        }

      default:
        break;
      }

    return lm_fail (lm, ERR_INTERNAL, "special form");
  }

ctl_if_:
  {
    Cell *form = payload.if_.form;

    return PUSH_SEQ (lm, eval_expr (CAR (form)),
                          select_branch (CDR (form)));
  }

ctl_if_cont:
  {
    Cell *form = payload.if_cont.form;
    Cell *pred = stk_pop (lm);

    if (!pred)
      return false;

    if (!cell_is_nil (pred))
      {
        // Lowered or returns this already evaluated predicate without a
        // temporary lexical binding or a second evaluation of the operand.
        if (CAR (form) == syntax_predicate_value)
          return stk_push (lm, pred);
        return ctl_push_state (lm, eval_expr (CAR (form)));
      }

    Cell *else_form = CAR (CDR (form));

    if (else_form)
      return ctl_push_state (lm, eval_expr (else_form));
    else
      return stk_push (lm, NIL);
  }
}

static EvaluationResult
lm_eval (LM *lm)
{
  size_t steps    = 0;
  size_t base_ptr = lm->ctl.sp;
  State  state;

  do
    {
      // A primitive can push NIL and set has_error while its VM step itself
      // succeeds. Check again AFTER the step: a tail-position primitive may
      // be the final step, with no later continuation to notice the error.
      if (lm->has_error || !ctl_pop (lm, &state)
          || !lm_eval_switch (lm, state.kind, state.payload) || lm->has_error)
        {
          lm_reset (lm);
          return (EvaluationResult){ .success = false, .value = NIL };
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

  Cell *value = stk_pop (lm);
  if (lm->has_error)
    {
      lm_reset (lm);
      return (EvaluationResult){ .success = false, .value = NIL };
    }
  return (EvaluationResult){ .success = true, .value = value };
}

LM *
lm_create (void)
{
  // Zeros out stack pointers
  LM *lm = xcalloc (1, sizeof *(lm));

  // Allocate pool
  lm->pool       = pool_init (LM_CAP_PER_POOL, sizeof (Cell));
  lm->env.global = lm->env.current = cell_make_environment (lm, NULL);

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

EvaluationResult
lm_evaluate (LM *lm, Cell *forms)
{
  // Measure each evaluation separately, including its initial progn state.
  lm->ctl.high_water = lm->ctl.sp;
  if (!ctl_push_state (lm, eval_body (forms)))
    {
      lm_reset (lm);
      return (EvaluationResult){ .success = false, .value = NIL };
    }

  return lm_eval (lm);
}

Cell *
lm_progn (LM *lm, Cell *progn)
{
  return lm_evaluate (lm, progn).value;
}
