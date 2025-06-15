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

static void
lm_reset (LM *lm)
{
  static const char *fmt
      = "Reset\nstk:0x%zX\nenv:0x%zX\nctl:0x%zX\ndmp:0x%zX\n";
  fprintf (stderr, fmt, lm->stk.sp, lm->env.sp, lm->ctl.sp, lm->dmp.sp);

  lm->stk.sp = lm->ctl.sp = lm->dmp.sp = 0;

  // Destroy every frame except the reserved global at index 0
  for (size_t i = 1; i < lm->env.sp; ++i)
    dict_destroy (lm->env.dict[i]);

  lm->env.sp = 1;
  lm->err_bool = false;
}

static bool
lm_dump (LM *lm)
{
  if ((lm)->dmp.sp >= LISPM_DUMP_MAX)
    return false;

  lm->dmp.dumps[(lm)->dmp.sp++] = (Dump){ .stk_sp = lm->stk.sp,
                                          .env_sp = lm->env.sp,
                                          .ctl_sp = lm->ctl.sp };
  LM_ENTER_FRAME (lm);

  return true;
overflow:
  return false; // TODO: set err here
}

static bool
lm_dump_restore (LM *lm)
{
  if (lm->dmp.sp == 0)
    return false;

  Dump dump = lm->dmp.dumps[--lm->dmp.sp];

  lm->stk.sp = dump.stk_sp;
  lm->env.sp = dump.env_sp;
  lm->ctl.sp = dump.ctl_sp;

  LM_LEAVE_FRAME (lm);

  return true;
underflow:
  return false; // TODO: set err here
}

static Cell *
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
      BAIL (lm, ERR_INTERNAL, "no such state");
    }

ctl_closure_enter:
  {
    LM_ENTER_FRAME (lm);
    goto next;
  }

ctl_closure_leave:
  {
    LM_LEAVE_FRAME (lm);
    goto next;
  }

ctl_eval:
  {
    Cell *expr = u.eval.expr ?: STK_POP (lm);

    if (IS_INST (expr, SYMBOL))
      STK_PUSH (lm, lm_env_lookup (lm, expr));
    else if (!LISTP (expr)) // literal
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
      BAIL (lm, ERR_INTERNAL, "eval");

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
      BAIL (lm, ERR_NOT_A_FUNCTION, "funcall");

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
        BAIL (lm, err, "lambda");
      }

    Cell *pairs = zip (lm, LIST2 (lambda->params, arglist, lm));

    while (!NILP (pairs))
      {
        Cell *pair = CAR (pairs);
        if (!lm_env_define (lm, (CAR (pair)), CADR (pair)))
          goto next;
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
          BAIL (lm, ERR_INVALID_ARG, "let: binding not a list");

        rev_vars = CONS (CAR (pair), rev_vars, lm);
        rev_exprs = CONS (CADR (pair), rev_exprs, lm);

        pairs = CDR (pairs);
      }

    Cell *vars = reverse_inplace (rev_vars);
    Cell *exprs = reverse_inplace (rev_exprs);

    CTL_PUSH (lm, closure_leave);
    CTL_PUSH (lm, lambda, LAMBDA (vars, progn, lm), NULL);
    CTL_PUSH (lm, closure_enter);
    CTL_PUSH (lm, evlis, NIL, exprs);

    goto next;
  }

ctl_define:
  {
    Cell *car = STK_POP (lm);
    Cell *cdr = STK_POP (lm);

    // TODO: check cdr len

    if (lm_env_define (lm, car, CAR (cdr)))
      STK_PUSH (lm, CAR (cdr));

    goto next;
  }

ctl_set:
  {
    Cell *car = STK_POP (lm);
    Cell *cdr = STK_POP (lm);

    // TODO: check cdr len

    if (lm_env_set (lm, car, CAR (cdr)))
      STK_PUSH (lm, CAR (cdr));

    goto next;
  }

ctl_apply:
  {
    Cell *fn = u.apply.fn ?: STK_POP (lm);
    Cell *arglist = u.apply.arglist ?: STK_POP (lm);

    if (!LISTP (arglist))
      BAIL (lm, ERR_MISSING_ARG, "apply: not a list.");

    Cell *fixed = butlast (lm, arglist);
    Cell *tail_list = CAR (last (lm, arglist));

    if (!LISTP (tail_list))
      BAIL (lm, ERR_MISSING_ARG, "apply: last not a list.");

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
      case THUNK_DEFINE:
        CTL_PUSH (lm, define);
        CTL_PUSH (lm, eval, CAR (u.lispm.arglist));
        CTL_PUSH (lm, evlis, NIL, CDR (u.lispm.arglist));
        break;
      case THUNK_SET:
        CTL_PUSH (lm, set);
        CTL_PUSH (lm, eval, CAR (u.lispm.arglist));
        CTL_PUSH (lm, evlis, NIL, CDR (u.lispm.arglist));
        break;
      case THUNK_LET:
        CTL_PUSH (lm, let, CAR (u.lispm.arglist));
        break;
      case THUNK_MAP:
        CTL_PUSH (lm, map, NULL, NULL);
        CTL_PUSH (lm, eval, CAR (u.lispm.arglist));
        CTL_PUSH (lm, evlis, NIL, CDR (u.lispm.arglist));
        break;
      case THUNK_GC:
        lm_gc (lm); // for now
        STK_PUSH (lm, T);
        break;
      default:
        BAIL (lm, ERR_INTERNAL, "lispm");
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

ctl_map:
  {
    Cell *fn = u.funcall.fn ?: STK_POP (lm);
    Cell *arglist = u.funcall.arglist ?: STK_POP (lm);

    CTL_PUSH (lm, map_cont, fn, NIL, zip (lm, arglist));

    goto next;
  }

ctl_map_cont:
  {
    if (NILP (u.map_cont.ziplist))
      {
        Cell *res = reverse_inplace (u.map_cont.acc);
        STK_PUSH (lm, res);
      }
    else
      {
        Cell *car = CAR (u.map_cont.ziplist);
        Cell *cdr = CDR (u.map_cont.ziplist);

        CTL_PUSH (lm, map_acc, u.map_cont.fn, u.map_cont.acc, cdr);
        CTL_PUSH (lm, funcall, u.map_cont.fn, car);
      }

    goto next;
  }

ctl_map_acc:
  {
    Cell *res = STK_POP (lm);
    Cell *acc = CONS (res, u.map_acc.acc, lm);

    CTL_PUSH (lm, map_cont, u.map_cont.fn, acc, u.map_acc.ziplist);

    goto next;
  }
next:
  {
    return T;
  }

  LM_ERR_HANDLERS (lm, LM_ERR_STATE (error) LM_ERR_STATE (overflow)
                           LM_ERR_STATE (underflow));
}

static Cell *
lm_eval (LM *lm)
{
  size_t base_ctl = lm->ctl.sp;

  do
    {
      if (lm->err_bool)
        goto error;

      State state = CTL_POP (lm);

      if (lm_eval_switch (lm, state.s, state.u) == NIL)
        return NIL;
    }
  while (base_ctl <= lm->ctl.sp);

  return STK_POP (lm);
  LM_ERR_HANDLERS (lm, LM_ERR_STATE (error) LM_ERR_STATE (underflow));
}

LM *
lm_create (void)
{
  CAR (NIL) = CDR (NIL) = NIL;

  LM *lm = xmalloc (sizeof *(lm));

  lm->stk.sp = 0;
  lm->env.sp = 0;
  lm->ctl.sp = 0;
  lm->dmp.sp = 0;

  lm->env.dict[lm->env.sp++] = dict_create (NULL, 0);

  lm->pool = pool_init (LM_OBJ_POOL_CAP, sizeof (Cell));

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

void
lm_err_set (LM *lm, ErrorCode code, const char *fmt, ...)
{
  va_list ap;
  lm->err_bool = true;

  fprintf (stderr, "%s: ", error_messages[code]);

  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);

  fputc ('\n', stderr);
}

Cell *
lm_progn (LM *lm, Cell *progn)
{
  CTL_PUSH (lm, progn, NIL, progn);
  return lm_eval (lm); // TODO: error detection

  LM_ERR_HANDLERS (lm, LM_ERR_STATE (overflow));
}
