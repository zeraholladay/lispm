#include "lispm.h"

#include <stdarg.h>
#include <stdbool.h>

#include "debug.h"
#include "env.h"
#include "error.h"
#include "eval.h"
#include "format.h"
#include "lispm_utils.h"
#include "types.h"
#include "xalloc.h"

#define POP(lm)                                                               \
  ({                                                                          \
    if ((lm)->stk.sp == 0)                                                    \
      goto underflow;                                                         \
    (lm)->stk.cells[--(lm)->stk.sp];                                          \
  })

#define PUSH(lm, val)                                                         \
  do                                                                          \
    {                                                                         \
      if ((lm)->stk.sp >= LISPM_STK_MAX)                                      \
        goto overflow;                                                        \
      (lm)->stk.cells[(lm)->stk.sp++] = val;                                  \
    }                                                                         \
  while (0)

#define STATE_POP(lm)                                                         \
  ({                                                                          \
    if ((lm)->ctl.sp == 0)                                                    \
      goto underflow;                                                         \
    (lm)->ctl.states[--(lm)->ctl.sp];                                         \
  })

#define STATE_PUSH(lm, tag, ...)                                              \
  do                                                                          \
    {                                                                         \
      if ((lm)->ctl.sp >= LISPM_CTL_MAX)                                      \
        goto overflow;                                                        \
      (lm)->ctl.states[(lm)->ctl.sp++]                                        \
          = (State){ .state = s_##tag, .uf_##tag = { __VA_ARGS__ } };         \
    }                                                                         \
  while (0)

#define LM_ERR(code, msg)                                                     \
  do                                                                          \
    {                                                                         \
      DEBUG (DEBUG_LOCATION);                                                 \
      fprintf (stderr, "[%s:%s] %s\n", #code, error_messages[code], msg);     \
      goto error;                                                             \
    }                                                                         \
  while (0);

typedef enum
{
#define X(tag, ...) s##tag,
#include "lispm.def"
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
#include "lispm.def"
#undef X
  };
} State;

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
  void *dmp; // temporary
  Pool *pool;
} LM;

static void
lm_reset (LM *lm)
{
  lm->stk.sp = lm->ctl.sp = 0;
  env_reset (lm->env);
}

static Cell *
lm_eval (LM *lm)
{
  while (lm->ctl.sp)
    {
      State s = STATE_POP (lm);
      switch (s.state)
        {
#define X(tag, ...)                                                           \
  case s##tag:                                                                \
    goto label##tag;
#include "lispm.def"
#undef STATE
        default:
          LM_ERR (ERR_INTERNAL, "No such state.");
        }
    label_env_enter_frame:
      {
        env_enter_frame (&lm->env);
        continue;
      }
    label_env_leave_frame:
      {
        env_leave_frame (&lm->env);
        continue;
      }
    label_eval:
      {
        Cell *arg = (s.uf_eval.arg) ? s.uf_eval.arg : POP (lm);

        if (IS (arg, SYMBOL))
          {
            PUSH (lm, lookup (arg, lm));
            continue;
          }

        // literals: numbers, strings, etc.
        if (!LISTP (arg))
          {
            PUSH (lm, arg);
            continue;
          }

        // cons and NIL
        if (LISTP (arg))
          {
            if (IS_NIL (arg))
              {
                PUSH (lm, NIL);
                continue;
              }

            Cell *car = CAR (arg);
            Cell *cdr = CDR (arg);

            if (car == KEYWORD (QUOTE))
              PUSH (lm, CAR (cdr));
            else if (IS (car, LAMBDA))
              PUSH (lm, car);
            else
              {
                STATE_PUSH (lm, eval_cont, cdr);
                STATE_PUSH (lm, eval, car);
              }
            continue;
          }
        LM_ERR (ERR_INTERNAL, "EVAL");
      }
    label_eval_cont:
      {
        Cell *fn = POP (lm);
        Cell *arglist = s.uf_eval.arg;

        // one of the fns this C code handles
        if (IS (fn, BUILTIN_FN) && fn->builtin_fn->is_lispm)
          {
            STATE_PUSH (lm, lispm, fn, arglist);
            continue;
          }

        STATE_PUSH (lm, funcall, fn, NULL);
        STATE_PUSH (lm, list, NIL, arglist);
        continue;
      }
    label_funcall:
      {
        Cell *fn = (s.uf_funcall.fn) ? (s.uf_funcall.fn) : POP (lm);
        Cell *arglist
            = (s.uf_funcall.arglist) ? (s.uf_funcall.arglist) : POP (lm);

        if (IS (fn, BUILTIN_FN))
          STATE_PUSH (lm, funcall_builtin, fn, arglist);
        else if (IS (fn, LAMBDA))
          STATE_PUSH (lm, funcall_lambda, fn, arglist);
        else
          LM_ERR (ERR_INTERNAL, "FUNCALL");
        continue;
      }
    label_funcall_builtin:
      {
        Cell *fn = s.uf_funcall_builtin.fn;
        Cell *arglist = s.uf_funcall_builtin.arglist;

        const BuiltinFn *builtin_fn = fn->builtin_fn;
        int received = (int)length (arglist);

        if (builtin_fn->arity > 0 && builtin_fn->arity != received)
          {
            ErrorCode err = (received < builtin_fn->arity)
                                ? ERR_MISSING_ARG
                                : ERR_UNEXPECTED_ARG;
            LM_ERR (err, builtin_fn->name);
          }

        if (!builtin_fn->fn)
          LM_ERR (ERR_NOT_A_FUNCTION, builtin_fn->name)

        Cell *res = builtin_fn->fn (arglist, lm);

        if (IS (res, ERROR))
          {
            PERROR (res);
            goto error;
          }

        PUSH (lm, res);
        continue;
      }
    label_funcall_lambda:
      {
        Cell *fn = s.uf_funcall_lambda.fn;
        Cell *arglist = s.uf_funcall_lambda.arglist;

        size_t expected = length (fn->lambda.params);
        size_t received = length (arglist);

        if (expected != received)
          {
            ErrorCode err
                = (received < expected) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
            LM_ERR (err, "LAMBDA");
          }

        STATE_PUSH (lm, env_leave_frame);
        STATE_PUSH (lm, funcall_lambda_cont, fn, arglist);
        STATE_PUSH (lm, env_enter_frame);
        continue;
      }
    label_funcall_lambda_cont:
      {
        Cell *fn = s.uf_funcall_lambda_cont.fn;
        Cell *arglist = s.uf_funcall_lambda_cont.arglist;

        Cell *pairs = mapcar (KEYWORD (LIST),
                              LIST2 (fn->lambda.params, arglist, lm), lm);

        while (!IS_NIL (pairs))
          {
            Cell *pair = CAR (pairs);
            lm_env_let (lm, (CAR (pair))->symbol.str, CADR (pair));
            pairs = CDR (pairs);
          }

        STATE_PUSH (lm, progn, NIL, fn->lambda.body);
        continue;
      }
    label_list:
      {
        Cell *rev = s.uf_list_acc.acc;
        Cell *arglist = s.uf_list.arglist;

        if (IS_NIL (arglist))
          {
            Cell *res = reverse_inplace (rev);
            PUSH (lm, res);
            continue;
          }

        Cell *car = CAR (arglist);
        Cell *cdr = CDR (arglist);

        STATE_PUSH (lm, list_acc, rev, cdr);
        STATE_PUSH (lm, eval, car);
        continue;
      }
    label_list_acc:
      {
        Cell *eval_res = POP (lm);

        Cell *old_rev = s.uf_list.acc;
        Cell *rev2 = CONS (eval_res, old_rev, lm);

        STATE_PUSH (lm, list, rev2, s.uf_list_acc.arglist);
        continue;
      }
    label_apply:
      {
        Cell *fn = s.uf_apply.fn ? s.uf_apply.fn : POP (lm);
        Cell *arglist = s.uf_apply.arglist ? s.uf_apply.arglist : POP (lm);

        if (!LISTP (arglist))
          LM_ERR (ERR_MISSING_ARG, "APPLY");

        Cell *fixd_args = butlast (arglist, lm);

        STATE_PUSH (lm, apply_cont, fn, arglist);
        STATE_PUSH (lm, list, NIL, fixd_args);
        continue;
      }
    label_apply_cont:
      {
        Cell *fixed_rev = POP (lm);
        Cell *fn = s.uf_apply_cont.fn;
        Cell *arglist = s.uf_apply_cont.arglist;
        Cell *tail_list = last (arglist, lm);

        if (!LISTP (tail_list)) // ie (apply fn NIL)
          STATE_PUSH (lm, funcall, fn, NIL);
        else
          {
            STATE_PUSH (lm, apply_funcall, fn, fixed_rev);
            STATE_PUSH (lm, eval, tail_list);
          }
        continue;
      }
    label_apply_funcall:
      {
        Cell *fn = s.uf_apply_funcall.fn;
        Cell *fixed_rev
            = s.uf_apply_funcall.arglist; // the reversed list of fixed‐values

        Cell *tail_list = POP (lm);

        if (!LISTP (tail_list))
          LM_ERR (ERR_MISSING_ARG, "FUNCALL");

        Cell *all_args = append_inplace (fixed_rev, tail_list);
        STATE_PUSH (lm, funcall, fn, all_args);
        continue;
      }
    label_lispm:
      {
        Cell *fn = s.uf_lispm.fn;
        Cell *arglist = s.uf_lispm.arglist;

        if (fn == KEYWORD (LIST))
          STATE_PUSH (lm, list, NIL, arglist);
        else if (fn == KEYWORD (FUNCALL))
          {
            STATE_PUSH (lm, funcall, NULL, CDR (arglist));
            STATE_PUSH (lm, eval, CAR (arglist));
          }
        else if (fn == KEYWORD (APPLY))
          {
            STATE_PUSH (lm, apply, NULL, CDR (arglist));
            STATE_PUSH (lm, eval, CAR (arglist));
          }
        else if (fn == KEYWORD (EVAL))
          {
            STATE_PUSH (lm, eval, NULL);
            STATE_PUSH (lm, eval, CAR (arglist));
          }
        else if (fn == KEYWORD (PROGN))
          STATE_PUSH (lm, progn, NIL, arglist);
        else if (fn == KEYWORD (AND))
          STATE_PUSH (lm, and, NIL, arglist);
        else if (fn == KEYWORD (OR))
          STATE_PUSH (lm, or, NIL, arglist);
        else if (fn == KEYWORD (IF))
          STATE_PUSH (lm, if, arglist);
        else
          LM_ERR (ERR_INTERNAL, "LISPM");
        continue;
      }
    label_progn:
      {
        Cell *res = s.uf_progn.res;
        Cell *arglist = s.uf_progn.arglist;

        if (IS_NIL (arglist))
          {
            PUSH (lm, res);
            continue;
          }

        STATE_PUSH (lm, progn_cont, res, arglist);
        STATE_PUSH (lm, eval, CAR (arglist));
        continue;
      }
    label_progn_cont:
      {
        Cell *arglist = s.uf_progn_cont.arglist;
        Cell *new_res = POP (lm);

        STATE_PUSH (lm, progn, new_res, CDR (arglist));
        continue;
      }
    label_if:
      {
        Cell *form = s.uf_if.form;
        Cell *pred_form = CAR (form);

        STATE_PUSH (lm, if_cont, form);
        STATE_PUSH (lm, eval, pred_form);
        continue;
      }

    label_if_cont:
      {
        Cell *pred_val = POP (lm);
        Cell *form = s.uf_if_cont.form;

        if (!IS_NIL (pred_val))
          {
            Cell *then_form = CAR (CDR (form));
            STATE_PUSH (lm, eval, then_form);
          }
        else
          {
            Cell *else_form = CAR (CDR (CDR (form)));
            if (else_form)
              STATE_PUSH (lm, eval, else_form);
            else
              PUSH (lm, NIL);
          }
        continue;
      }
    label_and:
      {
        Cell *arglist = s.uf_and.arglist;

        if (IS_NIL (arglist))
          {
            PUSH (lm, T);
            continue;
          }

        STATE_PUSH (lm, and_cont, NIL, arglist);
        STATE_PUSH (lm, eval, CAR (arglist));
        continue;
      }
    label_and_cont:
      {
        Cell *eval_res = POP (lm);
        Cell *arglist = s.uf_and_cont.arglist;

        if (IS_NIL (eval_res))
          {
            PUSH (lm, NIL);
            continue;
          }

        Cell *cdr = CDR (arglist);

        if (IS_NIL (cdr))
          {
            PUSH (lm, eval_res);
            continue;
          }

        STATE_PUSH (lm, and_cont, eval_res, cdr);
        STATE_PUSH (lm, eval, CAR (cdr));
        continue;
      }
    label_or:
      {
        Cell *arglist = s.uf_or.arglist;

        if (IS_NIL (arglist))
          {
            PUSH (lm, NIL);
            continue;
          }

        STATE_PUSH (lm, or_cont, NIL, arglist);
        STATE_PUSH (lm, eval, CAR (arglist));
        continue;
      }
    label_or_cont:
      {
        Cell *eval_res = POP (lm);
        Cell *arglist = s.uf_or_cont.arglist;

        if (!IS_NIL (eval_res))
          {
            PUSH (lm, eval_res);
            continue;
          }

        Cell *cdr = CDR (arglist);

        if (IS_NIL (cdr))
          {
            PUSH (lm, eval_res);
            continue;
          }

        STATE_PUSH (lm, or_cont, eval_res, cdr);
        STATE_PUSH (lm, eval, CAR (cdr));
        continue;
      }
    }

  Cell *evl_ret = POP (lm);
  return evl_ret;

error:;
  fputs ("**Error\n", stderr);
  return NIL;

underflow:;
  fputs ("**Underflow\n", stderr);
  return NIL;

overflow:;
  fputs ("**Overflow\n", stderr);
  return NIL;
}

LM *
lm_create (void)
{
  // fixme
  Cell *nil = KEYWORD (NIL);
  CAR (nil) = CDR (nil) = nil;

  LM *lm = xmalloc (sizeof *(lm));

  lm->stk.sp = lm->ctl.sp = 0;

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
lm_progn (LM *lm, Cell *progn)
{
  STATE_PUSH (lm, progn, NIL, progn);

  Cell *ret = lm_eval (lm);
  return ret;

overflow:
  lm_reset (lm);
  return NIL;
}
