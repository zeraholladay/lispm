#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

#include "dict.h"
#include "keywords.h"
#include "lm.h"
#include "lm_env.h"
#include "lm_gc.h"
#include "lm_secd.h"
#include "palloc.h"
#include "prims.h"
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
    {
      lm_err (lm, ERR_UNDERFLOW, "stk_pop");
      return NULL;
    }
  return lm->stk.cells[--lm->stk.sp];
}

static inline bool
stk_push (LM *lm, Cell *val)
{
  if (lm->stk.sp >= LISPM_STK_MAX)
    return lm_err (lm, ERR_OVERFLOW, "stk_push");

  lm->stk.cells[lm->stk.sp++] = val;
  return true;
}

static inline bool
enter_frame (LM *lm)
{
  if (lm->env.sp >= LISPM_ENV_MAX)
    return lm_err (lm, ERR_OVERFLOW, "enter_frame");

  lm->env.dict[lm->env.sp++] = dict_create (NULL, 0);
  return true;
}

static inline bool
leave_frame (LM *lm)
{
  if (lm->env.sp == 0)
    return lm_err (lm, ERR_UNDERFLOW, "leave_frame");

  lm->env.sp--;
  dict_destroy (lm->env.dict[lm->env.sp]);
  return true;
}

static inline bool
ctl_pop (LM *lm, State *out)
{
  if (lm->ctl.sp == 0)
    return lm_err (lm, ERR_UNDERFLOW, "ctl_pop");

  *out = lm->ctl.states[--lm->ctl.sp];
  return true;
}

static inline bool
ctl_push_state (LM *lm, State st)
{
  if (lm->ctl.sp >= LISPM_CTL_MAX)
    return lm_err (lm, ERR_OVERFLOW, "ctl_push_state");

  lm->ctl.states[lm->ctl.sp++] = st;
  return true;
}

static inline bool
ctl_push_states (LM *lm, const State sts[], size_t n)
{
  while (n-- > 0)
    {
      if (!ctl_push_state (lm, sts[n]))
        return false;
    }
  return true;
}

static bool
dump_push (LM *lm)
{
  if ((lm)->dmp.sp >= LISPM_DMP_MAX)
    return false;

  lm->dmp.dumps[(lm)->dmp.sp++] = (Dump){ .stk_sp = lm->stk.sp,
                                          .env_sp = lm->env.sp,
                                          .ctl_sp = lm->ctl.sp };
  if (!enter_frame (lm))
    return false;

  return true;
}

static bool
dump_pop (LM *lm)
{
  if (lm->dmp.sp == 0)
    return false;

  Dump dump = lm->dmp.dumps[--lm->dmp.sp];

  lm->stk.sp = dump.stk_sp;
  lm->env.sp = dump.env_sp;
  lm->ctl.sp = dump.ctl_sp;

  if (!leave_frame (lm))
    return false;

  return true;
}

static void
lm_reset (LM *lm)
{
  fprintf (stderr,
           "Registers:\n"
           "  stk: %zu\n"
           "  env: %zu\n"
           "  ctl: %zu\n"
           "  dmp: %zu\n",
           lm->stk.sp, lm->env.sp, lm->ctl.sp, lm->dmp.sp);

  // Destroy every frame except the reserved global at index 0
  for (size_t i = 1; i < lm->env.sp; ++i)
    dict_destroy (lm->env.dict[i]);

  // Reset stack pointers
  lm->stk.sp = 0;
  lm->env.sp = 1; // save global env
  lm->ctl.sp = 0;
  lm->dmp.sp = 0;

  lm->err_bool = false;
}

static bool
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
      return lm_err (lm, ERR_INTERNAL, "no such state");
    }

ctl_closure_enter:
  {
    return enter_frame (lm);
  }

ctl_closure_leave:
  {
    return leave_frame (lm);
  }

ctl_eval:
  {
    Cell *expr = u.eval.expr;

    if (!expr && !(expr = stk_pop (lm)))
      return false;

    if (IS_INST (expr, SYMBOL))
      return stk_push (lm, lm_env_lookup (lm, expr));

    if (!LISTP (expr)) // literal
      return stk_push (lm, expr);

    if (LISTP (expr))
      {
        if (NILP (expr))
          return stk_push (lm, NIL);

        Cell *car = CAR (expr);
        Cell *cdr = CDR (expr);

        if (car == QUOTE)
          return stk_push (lm, CAR (cdr));

        if (IS_INST (car, LAMBDA))
          return stk_push (lm, car);

        return ctl_push_state (lm, S (eval_apply, .fn = car, .arglist = cdr));
      }

    return lm_err (lm, ERR_INTERNAL, "eval");
  }

ctl_eval_apply:
  {
    Cell *fn      = u.eval_apply.fn;
    Cell *arglist = u.eval_apply.arglist;

    if (fn)
      return ctl_push_states (
          lm,
          (State[]){
              S (eval, .expr = fn),
              S (eval_apply, .fn = NULL, .arglist = arglist),
          },
          2);

    if (!(fn = stk_pop (lm)))
      return false;

    // one of the fns this C code handles
    if (IS_INST (fn, THUNK) && thunk_is_lispm (fn))
      return ctl_push_state (lm, S (lispm, .fn = fn, .arglist = arglist));

    return ctl_push_states (lm,
                            (State[]){
                                S (evlis, .acc = NIL, .arglist = arglist),
                                S (funcall, .fn = fn, .arglist = NULL),
                            },
                            2);
  }

ctl_evlis:
  {
    Cell *acc     = u.evlis.acc;
    Cell *arglist = u.evlis.arglist;

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

    if (IS_INST (fn, THUNK))
      return stk_push (lm, thunker (lm, fn, arglist));

    if (IS_INST (fn, LAMBDA))
      return ctl_push_states (lm,
                              (State[]){
                                  S (closure_enter),
                                  S (lambda, .fn = fn, .arglist = arglist),
                                  S (closure_leave),
                              },
                              3);

    return lm_err (lm, ERR_NOT_A_FUNCTION, "funcall");
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

    size_t expected = length (lambda->params);
    size_t received = length (arglist);

    if (expected != received)
      {
        ErrorCode err
            = (received < expected) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
        return lm_err (lm, err, "lambda");
      }

    Cell *pairs = zip (lm, LIST2 (lambda->params, arglist, lm));

    while (!NILP (pairs))
      {
        Cell *pair = CAR (pairs);

        if (!lm_env_define (lm, (CAR (pair)), CADR (pair)))
          return false;

        pairs = CDR (pairs);
      }

    return ctl_push_state (lm, S (progn, .res = NIL, .arglist = lambda->body));
  }

ctl_let:
  {
    Cell *progn = CDR (u.let.arglist);
    Cell *pairs = CAR (u.let.arglist);

    Cell *rev_vars  = NIL;
    Cell *rev_exprs = NIL;

    while (!NILP (pairs))
      {
        Cell *pair = CAR (pairs);

        if (!CONSP (pair))
          return lm_err (lm, ERR_INVALID_ARG, "let: binding not a list");

        rev_vars  = CONS (CAR (pair), rev_vars, lm);
        rev_exprs = CONS (CADR (pair), rev_exprs, lm);

        pairs = CDR (pairs);
      }

    Cell *vars  = reverse_inplace (rev_vars);
    Cell *exprs = reverse_inplace (rev_exprs);

    return ctl_push_states (
        lm,
        (State[]){
            S (evlis, .acc = NIL, .arglist = exprs),
            S (closure_enter),
            S (lambda, .fn = LAMBDA (vars, progn, lm), .arglist = NULL),
            S (closure_leave),
        },
        4);
  }

ctl_define:
  {
    Cell *car = stk_pop (lm);
    Cell *cdr = stk_pop (lm);

    if (!car || !cdr)
      return false;

    // TODO: validate pop & check cdr len

    if (!lm_env_define (lm, car, CAR (cdr)))
      return false;

    return stk_push (lm, CAR (cdr));
  }

ctl_set:
  {
    Cell *car = stk_pop (lm);
    Cell *cdr = stk_pop (lm);

    if (!car || !cdr)
      return false;

    // TODO: check cdr len

    if (!lm_env_set (lm, car, CAR (cdr)))
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

    if (!LISTP (arglist))
      return lm_err (lm, ERR_MISSING_ARG, "apply: not a list.");

    Cell *fixed     = butlast (lm, arglist);
    Cell *tail_list = CAR (last (lm, arglist));

    if (!LISTP (tail_list))
      return lm_err (lm, ERR_MISSING_ARG, "apply: last not a list.");

    Cell *all = append_inplace (fixed, tail_list);

    return ctl_push_state (lm, S (funcall, .fn = fn, .arglist = all));
  }

ctl_progn:
  {
    Cell *res     = u.progn.res;
    Cell *arglist = u.progn.arglist;

    if (NILP (arglist))
      return stk_push (lm, res);

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
        return ctl_push_states (
            lm,
            (State[]){
                S (evlis, .acc = NIL, .arglist = CDR (arglist)),
                S (eval, .expr = CAR (arglist)),
                S (define),
            },
            3);

      case THUNK_SET:
        return ctl_push_states (
            lm,
            (State[]){
                S (evlis, .acc = NIL, .arglist = CDR (arglist)),
                S (eval, .expr = CAR (arglist)),
                S (set),
            },
            3);

      case THUNK_MAP:
        return ctl_push_states (
            lm,
            (State[]){
                S (evlis, .acc = NIL, .arglist = CDR (arglist)),
                S (eval, .expr = CAR (arglist)),
                S (map, .fn = NULL, .arglist = NULL),
            },
            3);

      case THUNK_EVAL:
        return ctl_push_states (lm,
                                (State[]){
                                    S (eval, .expr = CAR (arglist)),
                                    S (eval, .expr = NULL),
                                },
                                2);

      case THUNK_LIST:
        return ctl_push_state (lm, S (evlis, .acc = NIL, .arglist = arglist));

      case THUNK_PROGN:
        return ctl_push_state (lm, S (progn, .res = NIL, .arglist = arglist));

      case THUNK_IF:
        return ctl_push_state (lm, S (if_, .form = arglist));

      case THUNK_AND:
        return ctl_push_state (lm, S (and, .arglist = arglist));

      case THUNK_OR:
        return ctl_push_state (lm, S (or, .arglist = arglist));

      case THUNK_LET:
        return ctl_push_state (lm, S (let, .arglist = CAR (arglist)));

      case THUNK_GC:
        lm_gc (lm); // for testing
        return stk_push (lm, T);

      default:
        break;
      }

    return lm_err (lm, ERR_INTERNAL, "lispm");
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
      return ctl_push_state (lm, S (eval, .expr = CAR (form)));

    Cell *else_form = CAR (CDR (form));

    if (else_form)
      return ctl_push_state (lm, S (eval, .expr = else_form));
    else
      return stk_push (lm, NIL);
  }

ctl_and:
  {
    Cell *arglist = u.and.arglist;

    if (NILP (arglist))
      return stk_push (lm, T);

    return ctl_push_states (lm,
                            (State[]){
                                S (eval, .expr = CAR (arglist)),
                                S (and_cont, .arglist = arglist),
                            },
                            2);
  }

ctl_and_cont:
  {
    Cell *res     = stk_pop (lm);
    Cell *arglist = u.and_cont.arglist;

    if (!res)
      return false;

    if (NILP (res))
      return stk_push (lm, NIL);

    Cell *cdr = CDR (arglist);

    if (NILP (cdr))
      return stk_push (lm, res);

    return ctl_push_states (lm,
                            (State[]){
                                S (eval, .expr = CAR (cdr)),
                                S (and_cont, .arglist = cdr),
                            },
                            2);
  }

ctl_or:
  {
    Cell *arglist = u.or.arglist;

    if (NILP (arglist))
      return stk_push (lm, NIL);

    return ctl_push_states (lm,
                            (State[]){
                                S (eval, .expr = CAR (arglist)),
                                S (or_cont, .arglist = arglist),
                            },
                            2);
  }

ctl_or_cont:
  {
    Cell *res     = stk_pop (lm);
    Cell *arglist = u.or_cont.arglist;

    if (!res)
      return false;

    if (!NILP (res))
      return stk_push (lm, res);

    Cell *cdr = CDR (arglist);

    if (NILP (cdr))
      return stk_push (lm, res);

    return ctl_push_states (lm,
                            (State[]){
                                S (eval, .expr = CAR (cdr)),
                                S (or_cont, .arglist = cdr),
                            },
                            2);
  }

ctl_map:
  {
    Cell *fn      = u.map.fn;
    Cell *arglist = u.map.arglist;

    if (!fn && !(fn = stk_pop (lm)))
      return false;

    if (!arglist && !(arglist = stk_pop (lm)))
      return false;

    Cell *ziplst = zip (lm, arglist);

    return ctl_push_state (
        lm, S (map_cont, .fn = fn, .acc = NIL, .ziplist = ziplst));
  }

ctl_map_cont:
  {
    Cell *fn     = u.map_cont.fn;
    Cell *acc    = u.map_cont.acc;
    Cell *zipped = u.map_cont.ziplist;

    if (NILP (zipped))
      {
        Cell *res = reverse_inplace (acc);
        return stk_push (lm, res);
      }

    Cell *car = CAR (zipped);
    Cell *cdr = CDR (zipped);

    return ctl_push_states (
        lm,
        (State[]){
            S (funcall, .fn = fn, .arglist = car),
            S (map_acc, .fn = fn, .acc = acc, .ziplist = cdr),
        },
        2);
  }

ctl_map_acc:
  {
    Cell *fn     = u.map_acc.fn;
    Cell *acc    = u.map_acc.acc;
    Cell *zipped = u.map_acc.ziplist;

    Cell *res = stk_pop (lm);
    if (!res)
      return false;

    Cell *acc2 = CONS (res, acc, lm);

    return ctl_push_state (
        lm, S (map_cont, .fn = fn, .acc = acc2, .ziplist = zipped));
  }
}

static Cell *
lm_eval (LM *lm)
{
  size_t base_ptr = lm->ctl.sp;
  State  state;

  do
    {
      if (lm->err_bool)
        {
          lm_reset (lm);
          return NIL;
        }

      if (!ctl_pop (lm, &state))
        {
          lm_reset (lm);
          return NIL;
        }

      if (!lm_eval_switch (lm, state.s, state.u))
        {
          lm_reset (lm);
          return NIL;
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

  // Create global frame
  lm->env.dict[lm->env.sp++] = dict_create (NULL, 0);

  // Allocate pool
  lm->pool = pool_init (LM_CAP_PER_POOL, sizeof (Cell));

  return lm;
}

void
lm_destroy (LM *lm)
{
  pool_destroy (&lm->pool);

  // Destory the whole environment
  for (size_t i = 0; i < lm->env.sp; ++i)
    dict_destroy (lm->env.dict[i]);

  free (lm);
}

Cell *
lm_alloc_cell (LM *lm)
{
  return pool_xalloc_hier (&lm->pool);
}

bool
lm_err (LM *lm, ErrorCode code, const char *fmt, ...)
{
  lm->err_bool = true;

  const char *msg = error_messages[code];
  fprintf (stderr, "***error: %s: ", msg);

  va_list ap;
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);

  fputc ('\n', stderr);

  return false;
}

Cell *
lm_progn (LM *lm, Cell *progn)
{
  if (ctl_push_state (lm, S (progn, .res = NIL, .arglist = progn)))
    return lm_eval (lm);

  lm_reset (lm);
  return NIL;
}
