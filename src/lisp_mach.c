#include <stdarg.h>
#include <stdbool.h>

#include "env.h"
#include "lisp_headers.h"
#include "lisp_mach.h"
#include "palloc.h"
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
          = (State){ .state = s_##tag, .uf_##tag = { __VA_ARGS__ } };         \
    }                                                                         \
  while (0)

#define ERR_EXIT(code, msg)                                                   \
  do                                                                          \
    {                                                                         \
      fprintf (stderr, "%s:%s %s\n", #code, error_messages[code], msg);       \
      goto error;                                                             \
    }                                                                         \
  while (0);

typedef enum
{
#define X(tag, ...) s##tag,
#include "lisp_mach.x-macros"
#undef X
  COUNT,
} StateEnum;

typedef struct state
{
  StateEnum state;
  union
  {
#define X(tag, ...)                                                           \
  struct                                                                      \
  {                                                                           \
    __VA_ARGS__;                                                              \
  } uf##tag;
#include "lisp_mach.x-macros"
#undef X
  };
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
} LM;

static void
lm_reset (LM *lm)
{
  static const char *fmt = "Reset\nstk:0x%zX\nctl:0x%zX\ndmp:0x%zX\n";

  fprintf (stderr, fmt, lm->stk.sp, lm->stk.sp, lm->dmp.sp);

  lm->stk.sp = lm->ctl.sp = lm->dmp.sp = 0;
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
lm_restore (LM *lm)
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
      State s = CTL_POP (lm);

      switch (s.state)
        {
#define X(tag, ...)                                                           \
  case s##tag:                                                                \
    goto state##tag;
#include "lisp_mach.x-macros"
#undef STATE
        default:
          ERR_EXIT (ERR_INTERNAL, "No such state.");
        }

    state_closure_leave:
      {
        env_enter_frame (&lm->env);
        goto next;
      }
    state_closure_enter:
      {
        env_leave_frame (&lm->env);
        goto next;
      }
    state_eval:
      {
        typeof (s.uf_eval) *st = &s.uf_eval;

        st->arg = st->arg ?: STK_POP (lm);

        if (IS_INST (st->arg, SYMBOL))
          STK_PUSH (lm, lookup (lm, st->arg));
        else if (!LISTP (st->arg))
          STK_PUSH (lm, st->arg);
        else if (LISTP (st->arg))
          {
            if (NILP (st->arg))
              STK_PUSH (lm, NIL);
            else
              {
                Cell *car = CAR (st->arg);
                Cell *cdr = CDR (st->arg);

                if (car == KEYWORD (QUOTE))
                  STK_PUSH (lm, CAR (cdr));
                else if (IS_INST (car, LAMBDA))
                  STK_PUSH (lm, car);
                else
                  CTL_PUSH (lm, eval_apply, car, cdr);
              }
          }
        else
          ERR_EXIT (ERR_INTERNAL, "eval");

        goto next;
      }
    state_eval_apply:
      {
        typeof (s.uf_eval_apply) *st = &s.uf_eval_apply;

        if (st->fn)
          {
            CTL_PUSH (lm, eval_apply, NULL, st->arglist);
            CTL_PUSH (lm, eval, st->fn);
          }
        else
          {
            Cell *fn = STK_POP (lm);

            // one of the fns this C code handles
            if (IS_INST (fn, BUILTIN_FN) && fn->builtin_fn->is_lispm)
              CTL_PUSH (lm, lispm, fn, st->arglist);
            else
              {
                CTL_PUSH (lm, funcall, fn, NULL);
                CTL_PUSH (lm, evlis, NIL, st->arglist);
              }
          }
        goto next;
      }
    state_evlis:
      {
        typeof (s.uf_evlis) *st = &s.uf_evlis;

        if (NILP (st->arglist))
          {
            Cell *res = reverse_inplace (st->acc);
            STK_PUSH (lm, res);
          }
        else
          {
            Cell *car = CAR (st->arglist);
            Cell *cdr = CDR (st->arglist);

            CTL_PUSH (lm, evlis_acc, st->acc, cdr);
            CTL_PUSH (lm, eval, car);
          }

        goto next;
      }
    state_evlis_acc:
      {
        typeof (s.uf_evlis_acc) *st = &s.uf_evlis_acc;

        Cell *eval_res = STK_POP (lm);
        Cell *acc = CONS (eval_res, st->acc, lm);

        CTL_PUSH (lm, evlis, acc, st->arglist);

        goto next;
      }
    state_funcall:
      {
        typeof (s.uf_funcall) *st = &s.uf_funcall;

        Cell *fn = st->fn ?: STK_POP (lm);
        Cell *arglist = st->arglist ?: STK_POP (lm);

        if (IS_INST (fn, BUILTIN_FN))
          CTL_PUSH (lm, funcall_builtin, fn, arglist);
        else if (IS_INST (fn, LAMBDA))
          {
            CTL_PUSH (lm, closure_leave);
            CTL_PUSH (lm, lambda, fn, arglist);
            CTL_PUSH (lm, closure_enter);
          }
        else
          ERR_EXIT (ERR_NOT_A_FUNCTION, "funcall");

        goto next;
      }
    state_funcall_builtin:
      {
        typeof (s.uf_funcall_builtin) *st = &s.uf_funcall_builtin;

        const BuiltinFn *builtin_fn = st->fn->builtin_fn;

        int received = (int)length (st->arglist);

        if (builtin_fn->arity > 0 && builtin_fn->arity != received)
          {
            ErrorCode err = (received < builtin_fn->arity)
                                ? ERR_MISSING_ARG
                                : ERR_UNEXPECTED_ARG;
            ERR_EXIT (err, builtin_fn->name);
          }

        if (!builtin_fn->fn)
          ERR_EXIT (ERR_NOT_A_FUNCTION, builtin_fn->name)

        Cell *res = builtin_fn->fn (lm, st->arglist);

        if (IS_INST (res, ERROR))
          {
            PERROR (res); // fixme
            goto error;
          }

        STK_PUSH (lm, res);

        goto next;
      }
    state_lambda:
      {
        typeof (s.uf_lambda) *st = &s.uf_lambda;

        Cell *fn = st->fn ?: STK_POP (lm);
        Cell *arglist = st->arglist ?: STK_POP (lm);

        Lambda *lambda = &fn->lambda;

        size_t expected = length (lambda->params);
        size_t received = length (arglist);

        if (expected != received)
          {
            ErrorCode err
                = (received < expected) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
            ERR_EXIT (err, "lambda");
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
    state_let:
      {
        typeof (s.uf_let) *st = &s.uf_let;

        Cell *progn = CDR (st->arglist);
        Cell *pairs = CAR (st->arglist);

        Cell *rev_vars = NIL;
        Cell *rev_exprs = NIL;

        while (!NILP (pairs))
          {
            Cell *pair = CAR (pairs);

            if (!CONSP (pair))
              ERR_EXIT (ERR_INVALID_ARG, "let: binding not a list");

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
    state_apply:
      {
        typeof (s.uf_apply) *st = &s.uf_apply;

        Cell *fn = st->fn ?: STK_POP (lm);
        Cell *arglist = st->arglist ?: STK_POP (lm);

        if (!LISTP (arglist))
          ERR_EXIT (ERR_MISSING_ARG, "apply: not a list.");

        Cell *fixed = butlast (lm, arglist);
        Cell *tail_list = CAR (last (lm, arglist));

        if (!LISTP (tail_list))
          ERR_EXIT (ERR_MISSING_ARG, "apply: last not a list.");

        Cell *all = append_inplace (fixed, tail_list);

        CTL_PUSH (lm, funcall, fn, all);

        goto next;
      }
    state_progn:
      {
        typeof (s.uf_progn) *st = &s.uf_progn;

        if (NILP (st->arglist))
          STK_PUSH (lm, st->res);
        else
          {
            CTL_PUSH (lm, progn_eval, CDR (st->arglist));
            CTL_PUSH (lm, eval, CAR (st->arglist));
          }

        goto next;
      }
    state_progn_eval:
      {
        typeof (s.uf_progn_eval) *st = &s.uf_progn_eval;

        Cell *eval_res = STK_POP (lm);
        CTL_PUSH (lm, progn, eval_res, st->arglist);

        goto next;
      }
    state_lispm:
      {
        typeof (s.uf_lispm) *st = &s.uf_lispm;

        if (st->fn == KEYWORD (LIST))
          CTL_PUSH (lm, evlis, NIL, st->arglist);
        else if (st->fn == KEYWORD (FUNCALL))
          {
            CTL_PUSH (lm, funcall, NULL, NULL);
            CTL_PUSH (lm, eval, CAR (st->arglist));
            CTL_PUSH (lm, evlis, NIL, CDR (st->arglist));
          }
        else if (st->fn == KEYWORD (APPLY))
          {
            CTL_PUSH (lm, apply, NULL, NULL);
            CTL_PUSH (lm, eval, CAR (st->arglist));
            CTL_PUSH (lm, evlis, NIL, CDR (st->arglist));
          }
        else if (st->fn == KEYWORD (EVAL))
          {
            CTL_PUSH (lm, eval, NULL);
            CTL_PUSH (lm, eval, CAR (st->arglist));
          }
        else if (st->fn == KEYWORD (PROGN))
          CTL_PUSH (lm, progn, NIL, st->arglist);
        else if (st->fn == KEYWORD (AND))
          CTL_PUSH (lm, and, st->arglist);
        else if (st->fn == KEYWORD (OR))
          CTL_PUSH (lm, or, st->arglist);
        else if (st->fn == KEYWORD (IF))
          CTL_PUSH (lm, if, st->arglist);
        else if (st->fn == KEYWORD (LET))
          CTL_PUSH (lm, let, CAR (st->arglist));
        else
          ERR_EXIT (ERR_INTERNAL, "lispm");

        goto next;
      }
    state_if:
      {
        typeof (s.uf_if) *st = &s.uf_if;

        CTL_PUSH (lm, if_cont, CDR (st->form));
        CTL_PUSH (lm, eval, CAR (st->form));

        goto next;
      }
    state_if_cont:
      {
        typeof (s.uf_if_cont) *st = &s.uf_if_cont;

        Cell *pred_val = STK_POP (lm);

        if (!NILP (pred_val))
          {
            Cell *then_form = CAR (st->form);
            CTL_PUSH (lm, eval, then_form);
          }
        else
          {
            Cell *else_form = CAR (CDR (st->form));
            if (else_form)
              CTL_PUSH (lm, eval, else_form);
            else
              STK_PUSH (lm, NIL);
          }

        goto next;
      }
    state_and:
      {
        typeof (s.uf_and) *st = &s.uf_and;

        if (NILP (st->arglist))
          STK_PUSH (lm, T);
        else
          {
            CTL_PUSH (lm, and_cont, st->arglist);
            CTL_PUSH (lm, eval, CAR (st->arglist));
          }

        goto next;
      }
    state_and_cont:
      {
        typeof (s.uf_and_cont) *st = &s.uf_and_cont;

        Cell *eval_res = STK_POP (lm);

        if (NILP (eval_res))
          STK_PUSH (lm, NIL);
        else
          {
            Cell *cdr = CDR (st->arglist);

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
    state_or:
      {
        typeof (s.uf_or) *st = &s.uf_or;

        if (NILP (st->arglist))
          STK_PUSH (lm, NIL);
        else
          {
            CTL_PUSH (lm, or_cont, st->arglist);
            CTL_PUSH (lm, eval, CAR (st->arglist));
          }

        goto next;
      }
    state_or_cont:
      {
        typeof (s.uf_or_cont) *st = &s.uf_or_cont;

        Cell *eval_res = STK_POP (lm);

        if (!NILP (eval_res))
          STK_PUSH (lm, eval_res);
        else
          {
            Cell *cdr = CDR (st->arglist);

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
  Cell *nil = KEYWORD (NIL);
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
