#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

#include "env.h"
#include "lm.h"
#include "palloc.h"
#include "prims.h"
#include "thunks.h"
#include "types.h"
#include "xalloc.h"

#define STK_POP(lm)                                                           \
  ({                                                                          \
    if ((lm)->stk.sp == 0)                                                    \
      goto underflow;                                                         \
    (lm)->stk.cells[--(lm)->stk.sp];                                          \
  })

#define STK_PUSH(lm, val)                                                     \
  do                                                                          \
    {                                                                         \
      if ((lm)->stk.sp >= LISPM_STK_MAX)                                      \
        goto overflow;                                                        \
      (lm)->stk.cells[(lm)->stk.sp++] = val;                                  \
    }                                                                         \
  while (0)

#define CTL_POP(lm)                                                           \
  ({                                                                          \
    if ((lm)->ctl.sp == 0)                                                    \
      goto underflow;                                                         \
    (lm)->ctl.states[--(lm)->ctl.sp];                                         \
  })

#define CTL_PUSH(lm, tag, ...)                                                \
  do                                                                          \
    {                                                                         \
      if ((lm)->ctl.sp >= LISPM_CTL_MAX)                                      \
        goto overflow;                                                        \
      (lm)->ctl.states[(lm)->ctl.sp++]                                        \
          = (State){ .state = s_##tag, .u.tag = { __VA_ARGS__ } };            \
    }                                                                         \
  while (0)

#define BAIL_ON_ERR(lm, err_code, msg)                                        \
  do                                                                          \
    {                                                                         \
      lm_err (lm, err_code, msg);                                             \
      goto error;                                                             \
    }                                                                         \
  while (0);

typedef enum
{
#define X(tag, ...) s_##tag,
#include "lm.def"
#undef X
  COUNT,
} StateEnum;

typedef union lisp_mach
{
#define X(tag, ...)                                                           \
  struct                                                                      \
  {                                                                           \
    __VA_ARGS__;                                                              \
  } tag;
#include "lm.def"
#undef X
} Union;

typedef struct state
{
  StateEnum state;
  Union u;
} State;

typedef struct dump
{
  size_t stk_sp, ctl_sp;
} Dump;

typedef struct lispm_secd
{
  struct
  {
    size_t sp;
    Cell *cells[LISPM_STK_MAX];
  } stk;
  Env *env;
  struct
  {
    size_t sp;
    State states[LISPM_CTL_MAX];
  } ctl;
  struct
  {
    size_t sp;
    Dump dumps[LISPM_DUMP_MAX];
  } dmp;
  Pool *pool;
  bool err_bool;
} LM;

static void
lm_reset (LM *lm)
{
  static const char *fmt = "Reset\nstk:0x%zX\nctl:0x%zX\ndmp:0x%zX\n";

  fprintf (stderr, fmt, lm->stk.sp, lm->stk.sp, lm->dmp.sp);

  lm->stk.sp = lm->ctl.sp = lm->dmp.sp = 0;
  lm->err_bool = false;
  env_reset (&lm->env);
}

static bool
lm_dump (LM *lm)
{
  if ((lm)->dmp.sp >= LISPM_DUMP_MAX)
    return false;

  lm->dmp.dumps[(lm)->dmp.sp++] = (Dump){
    .stk_sp = lm->stk.sp,
    .ctl_sp = lm->ctl.sp,
  };

  env_enter_frame (&lm->env);

  return true;
}

static bool
lm_dump_restore (LM *lm)
{
  if (lm->dmp.sp == 0)
    return false;

  Dump dump = lm->dmp.dumps[--lm->dmp.sp];

  lm->stk.sp = dump.stk_sp;
  lm->ctl.sp = dump.ctl_sp;

  env_leave_frame (&lm->env);

  return true;
}

static Cell *
lm_eval (LM *lm)
{
  size_t base_ctl = lm->ctl.sp;

  do
    {
      if (lm->err_bool)
        goto error;

      State s = CTL_POP (lm);
      Union u = s.u;

      switch (s.state)
        {
#define X(tag, ...)                                                           \
  case s_##tag:                                                               \
    goto ctl_##tag;
#include "lm.def"
#undef STATE
        default:
          BAIL_ON_ERR (lm, ERR_INTERNAL, "No such state.");
        }

    ctl_closure_leave:
      {
        env_enter_frame (&lm->env);
        goto next;
      }
    ctl_closure_enter:
      {
        env_leave_frame (&lm->env);
        goto next;
      }
    ctl_eval:
      {
        Cell *expr = u.eval.expr ?: STK_POP (lm);

        if (IS_INST (expr, SYMBOL))
          STK_PUSH (lm, lookup (lm, expr));
        else if (!LISTP (expr))
          STK_PUSH (lm, expr);
        else if (LISTP (expr))
          {
            if (NILP (expr))
              STK_PUSH (lm, NIL);
            else
              {
                Cell *car = CAR (expr);
                Cell *cdr = CDR (expr);

                if (car == QUOTE)
                  STK_PUSH (lm, CAR (cdr));
                else if (IS_INST (car, LAMBDA))
                  STK_PUSH (lm, car);
                else
                  CTL_PUSH (lm, eval_apply, car, cdr);
              }
          }
        else
          BAIL_ON_ERR (lm, ERR_INTERNAL, "eval");

        goto next;
      }
    ctl_eval_apply:
      {
        if (u.eval_apply.fn)
          {
            CTL_PUSH (lm, eval_apply, NULL, u.eval_apply.arglist);
            CTL_PUSH (lm, eval, u.eval_apply.fn);
          }
        else
          {
            Cell *fn = STK_POP (lm);

            // one of the fns this C code handles
            if (IS_INST (fn, THUNK) && thunk_is_lispm (fn))
              CTL_PUSH (lm, lispm, fn, u.eval_apply.arglist);
            else
              {
                CTL_PUSH (lm, funcall, fn, NULL);
                CTL_PUSH (lm, evlis, NIL, u.eval_apply.arglist);
              }
          }
        goto next;
      }
    ctl_evlis:
      {
        if (NILP (u.evlis.arglist))
          {
            Cell *res = reverse_inplace (u.evlis.acc);
            STK_PUSH (lm, res);
          }
        else
          {
            Cell *car = CAR (u.evlis.arglist);
            Cell *cdr = CDR (u.evlis.arglist);

            CTL_PUSH (lm, evlis_acc, u.evlis.acc, cdr);
            CTL_PUSH (lm, eval, car);
          }

        goto next;
      }
    ctl_evlis_acc:
      {
        Cell *eval_res = STK_POP (lm);
        Cell *acc = CONS (eval_res, u.evlis_acc.acc, lm);

        CTL_PUSH (lm, evlis, acc, u.evlis_acc.arglist);

        goto next;
      }
    ctl_funcall:
      {
        Cell *fn = u.funcall.fn ?: STK_POP (lm);
        Cell *arglist = u.funcall.arglist ?: STK_POP (lm);

        if (IS_INST (fn, THUNK))
          {
            Cell *ret = thunker (lm, fn, arglist);
            STK_PUSH (lm, ret);
          }
        else if (IS_INST (fn, LAMBDA))
          {
            CTL_PUSH (lm, closure_leave);
            CTL_PUSH (lm, lambda, fn, arglist);
            CTL_PUSH (lm, closure_enter);
          }
        else
          BAIL_ON_ERR (lm, ERR_NOT_A_FUNCTION, "funcall");

        goto next;
      }
    ctl_lambda:
      {
        Cell *fn = u.lambda.fn ?: STK_POP (lm);
        Cell *arglist = u.lambda.arglist ?: STK_POP (lm);

        Lambda *lambda = &fn->lambda;

        size_t expected = length (lambda->params);
        size_t received = length (arglist);

        if (expected != received)
          {
            ErrorCode err
                = (received < expected) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
            BAIL_ON_ERR (lm, err, "lambda");
          }

        Cell *pairs = zip (lm, LIST2 (lambda->params, arglist, lm));

        while (!NILP (pairs))
          {
            Cell *pair = CAR (pairs);
            lm_env_let (lm, (CAR (pair))->symbol.str, CADR (pair));
            pairs = CDR (pairs);
          }

        CTL_PUSH (lm, progn, NIL, lambda->body);

        goto next;
      }
    ctl_let:
      {
        Cell *progn = CDR (u.let.arglist);
        Cell *pairs = CAR (u.let.arglist);

        Cell *rev_vars = NIL;
        Cell *rev_exprs = NIL;

        while (!NILP (pairs))
          {
            Cell *pair = CAR (pairs);

            if (!CONSP (pair))
              BAIL_ON_ERR (lm, ERR_INVALID_ARG, "let: binding not a list");

            rev_vars = CONS (CAR (pair), rev_vars, lm);
            rev_exprs = CONS (CADR (pair), rev_exprs, lm);

            pairs = CDR (pairs);
          }

        Cell *vars = reverse_inplace (rev_vars);
        Cell *exprs = reverse_inplace (rev_exprs);

        Cell *fn = LAMBDA (vars, progn, lm);

        CTL_PUSH (lm, closure_leave);
        CTL_PUSH (lm, lambda, fn, NULL);
        CTL_PUSH (lm, closure_enter);
        CTL_PUSH (lm, evlis, NIL, exprs);

        goto next;
      }
    ctl_apply:
      {
        Cell *fn = u.apply.fn ?: STK_POP (lm);
        Cell *arglist = u.apply.arglist ?: STK_POP (lm);

        if (!LISTP (arglist))
          BAIL_ON_ERR (lm, ERR_MISSING_ARG, "apply: not a list.");

        Cell *fixed = butlast (lm, arglist);
        Cell *tail_list = CAR (last (lm, arglist));

        if (!LISTP (tail_list))
          BAIL_ON_ERR (lm, ERR_MISSING_ARG, "apply: last not a list.");

        Cell *all = append_inplace (fixed, tail_list);

        CTL_PUSH (lm, funcall, fn, all);

        goto next;
      }
    ctl_progn:
      {
        if (NILP (u.progn.arglist))
          STK_PUSH (lm, u.progn.res);
        else
          {
            CTL_PUSH (lm, progn_eval, CDR (u.progn.arglist));
            CTL_PUSH (lm, eval, CAR (u.progn.arglist));
          }

        goto next;
      }
    ctl_progn_eval:
      {
        Cell *eval_res = STK_POP (lm);

        CTL_PUSH (lm, progn, eval_res, u.progn_eval.arglist);

        goto next;
      }
    ctl_lispm:
      {
        Cell *fn = u.lispm.fn;

        switch (fn->thunk)
          {
          case THUNK_APPLY:
            CTL_PUSH (lm, apply, NULL, NULL);
            CTL_PUSH (lm, eval, CAR (u.lispm.arglist));
            CTL_PUSH (lm, evlis, NIL, CDR (u.lispm.arglist));
            break;
          case THUNK_FUNCALL:
            CTL_PUSH (lm, funcall, NULL, NULL);
            CTL_PUSH (lm, eval, CAR (u.lispm.arglist));
            CTL_PUSH (lm, evlis, NIL, CDR (u.lispm.arglist));
            break;
          case THUNK_EVAL:
            CTL_PUSH (lm, eval, NULL);
            CTL_PUSH (lm, eval, CAR (u.lispm.arglist));
            break;
          case THUNK_LIST:
            CTL_PUSH (lm, evlis, NIL, u.lispm.arglist);
            break;
          case THUNK_PROGN:
            CTL_PUSH (lm, progn, NIL, u.lispm.arglist);
            break;
          case THUNK_IF:
            CTL_PUSH (lm, if_, u.lispm.arglist);
            break;
          case THUNK_AND:
            CTL_PUSH (lm, and, u.lispm.arglist);
            break;
          case THUNK_OR:
            CTL_PUSH (lm, or, u.lispm.arglist);
            break;
          case THUNK_LET:
            CTL_PUSH (lm, let, CAR (u.lispm.arglist));
            break;
          default:
            BAIL_ON_ERR (lm, ERR_INTERNAL, "lispm");
          }

        goto next;
      }
    ctl_if_:
      {
        CTL_PUSH (lm, if_cont, CDR (u.if_.form));
        CTL_PUSH (lm, eval, CAR (u.if_.form));

        goto next;
      }
    ctl_if_cont:
      {
        Cell *pred_val = STK_POP (lm);

        if (!NILP (pred_val))
          {
            Cell *then_form = CAR (u.if_cont.form);
            CTL_PUSH (lm, eval, then_form);
          }
        else
          {
            Cell *else_form = CAR (CDR (u.if_cont.form));
            if (else_form)
              CTL_PUSH (lm, eval, else_form);
            else
              STK_PUSH (lm, NIL);
          }

        goto next;
      }
    ctl_and:
      {
        if (NILP (u.and.arglist))
          STK_PUSH (lm, T);
        else
          {
            CTL_PUSH (lm, and_cont, u.and.arglist);
            CTL_PUSH (lm, eval, CAR (u.and.arglist));
          }

        goto next;
      }
    ctl_and_cont:
      {
        Cell *eval_res = STK_POP (lm);

        if (NILP (eval_res))
          STK_PUSH (lm, NIL);
        else
          {
            Cell *cdr = CDR (u.and.arglist);

            if (NILP (cdr))
              STK_PUSH (lm, eval_res);
            else
              {
                CTL_PUSH (lm, and_cont, cdr);
                CTL_PUSH (lm, eval, CAR (cdr));
              }
          }

        goto next;
      }
    ctl_or:
      {
        if (NILP (u.or.arglist))
          STK_PUSH (lm, NIL);
        else
          {
            CTL_PUSH (lm, or_cont, u.or.arglist);
            CTL_PUSH (lm, eval, CAR (u.or.arglist));
          }

        goto next;
      }
    ctl_or_cont:
      {
        Cell *eval_res = STK_POP (lm);

        if (!NILP (eval_res))
          STK_PUSH (lm, eval_res);
        else
          {
            Cell *cdr = CDR (u.or_cont.arglist);

            if (NILP (cdr))
              STK_PUSH (lm, eval_res);
            else
              {
                CTL_PUSH (lm, or_cont, cdr);
                CTL_PUSH (lm, eval, CAR (cdr));
              }
          }

        goto next;
      }
    next:;
    }
  while (base_ctl <= lm->ctl.sp);

  return STK_POP (lm);

error:;
  fputs ("*** Error:", stderr);
  goto reset;

underflow:;
  fputs ("*** Underflow:", stderr);
  goto reset;

overflow:;
  fputs ("*** Overflow:", stderr);
  goto reset;

reset:
  lm_reset (lm);
  return NIL;
}

LM *
lm_create (void)
{
  // fixme
  Cell *nil = NIL;
  CAR (nil) = CDR (nil) = nil;

  LM *lm = xmalloc (sizeof *(lm));

  lm->stk.sp = lm->ctl.sp = lm->dmp.sp = 0;

  lm->env = env_create ();
  lm->pool = pool_init (LM_OBJ_POOL_CAP, sizeof (Cell));

  return lm;
}

void
lm_destroy (LM *lm)
{
  env_destroy (lm->env);
  pool_destroy (&lm->pool);
  free (lm);
}

Cell *
lm_alloc_cell (LM *lm)
{
  return pool_xalloc_hier (&lm->pool);
}

Cell *
lm_env_lkup (LM *lm, const char *key)
{
  return env_lookup (lm->env, key);
}

bool
lm_env_let (LM *lm, const char *key, Cell *val)
{
  return env_let (lm->env, key, val);
}

bool
lm_env_set (LM *lm, const char *key, Cell *val)
{
  return env_set (lm->env, key, val);
}

void
lm_err (LM *lm, ErrorCode code, const char *msg)
{
  lm->err_bool = true;
  fprintf (stderr, "%s: %s\n", error_messages[code], msg);
}

Cell *
lm_funcall (LM *lm, Cell *fn, Cell *arglist)
{
  CTL_PUSH (lm, funcall, fn, arglist);
  return lm_eval (lm); // TODO: error detection

overflow:
  fputs ("*** Overflow:", stderr);
  lm_reset (lm);
  return NIL;
}

Cell *
lm_progn (LM *lm, Cell *progn)
{
  CTL_PUSH (lm, progn, NIL, progn);
  return lm_eval (lm); // TODO: error detection

overflow:
  fputs ("*** Overflow:", stderr);
  lm_reset (lm);
  return NIL;
}
