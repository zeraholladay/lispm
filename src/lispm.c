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

#define SPOP(lm)                                                              \
  ({                                                                          \
    if ((lm)->ctl.sp == 0)                                                    \
      goto underflow;                                                         \
    (lm)->ctl.states[--(lm)->ctl.sp];                                         \
  })

#define SPUSH(lm, tag, ...)                                                   \
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
      State s = SPOP (lm);
      switch (s.state)
        {
#define X(tag, ...)                                                           \
  case s##tag:                                                                \
    goto tag;
#include "lispm.def"
#undef STATE
        default:
          LM_ERR (ERR_INTERNAL, "No such state.");
        }
    _env_enter_frame:
      {
        env_enter_frame (&lm->env);
        continue;
      }
    _env_leave_frame:
      {
        env_leave_frame (&lm->env);
        continue;
      }
    _eval:
      {
        typeof (s.uf_eval) *st = &s.uf_eval;

        st->arg = st->arg ?: POP (lm);

        if (IS_INST (st->arg, SYMBOL))
          {
            PUSH (lm, lookup (st->arg, lm));
            continue;
          }

        // literals: numbers, strings, etc.
        if (!LISTP (st->arg))
          {
            PUSH (lm, st->arg);
            continue;
          }

        // cons and NIL
        if (LISTP (st->arg))
          {
            if (IS_NIL (st->arg))
              {
                PUSH (lm, NIL);
                continue;
              }

            Cell *car = CAR (st->arg);
            Cell *cdr = CDR (st->arg);

            if (car == KEYWORD (QUOTE))
              PUSH (lm, CAR (cdr));
            else if (IS_INST (car, LAMBDA))
              PUSH (lm, car);
            else
              {
                SPUSH (lm, eval_cont, cdr);
                SPUSH (lm, eval, car);
              }
            continue;
          }
        LM_ERR (ERR_INTERNAL, "EVAL");
      }
    _eval_cont:
      {
        typeof (s.uf_eval_cont) *st = &s.uf_eval_cont;

        Cell *fn = POP (lm);

        // one of the fns this C code handles
        if (IS_INST (fn, BUILTIN_FN) && fn->builtin_fn->is_lispm)
          SPUSH (lm, lispm, fn, st->arglist);
        else
          {
            SPUSH (lm, funcall, fn, NULL);
            SPUSH (lm, list, NIL, st->arglist);
          }
        continue;
      }
    _funcall:
      {
        typeof (s.uf_funcall) *st = &s.uf_funcall;

        Cell *fn = st->fn ?: POP (lm);
        Cell *arglist = st->arglist ?: POP (lm);

        if (IS_INST (fn, BUILTIN_FN))
          SPUSH (lm, funcall_builtin, fn, arglist);
        else if (IS_INST (fn, LAMBDA))
          SPUSH (lm, funcall_lambda, fn, arglist);
        else
          LM_ERR (ERR_INTERNAL, "FUNCALL");
        continue;
      }
    _funcall_builtin:
      {
        typeof (s.uf_funcall_builtin) *st = &s.uf_funcall_builtin;

        const BuiltinFn *builtin_fn = st->fn->builtin_fn;

        int received = (int)length (st->arglist);

        if (builtin_fn->arity > 0 && builtin_fn->arity != received)
          {
            ErrorCode err = (received < builtin_fn->arity)
                                ? ERR_MISSING_ARG
                                : ERR_UNEXPECTED_ARG;
            LM_ERR (err, builtin_fn->name);
          }

        if (!builtin_fn->fn)
          LM_ERR (ERR_NOT_A_FUNCTION, builtin_fn->name)

        Cell *res = builtin_fn->fn (st->arglist, lm);

        if (IS_INST (res, ERROR))
          {
            PERROR (res);
            goto error;
          }

        PUSH (lm, res);
        continue;
      }
    _funcall_lambda:
      {
        typeof (s.uf_funcall_lambda) *st = &s.uf_funcall_lambda;

        size_t expected = length (st->fn->lambda.params);
        size_t received = length (st->arglist);

        if (expected != received)
          {
            ErrorCode err
                = (received < expected) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
            LM_ERR (err, "LAMBDA");
          }

        SPUSH (lm, env_leave_frame);
        SPUSH (lm, funcall_lambda_cont, st->fn, st->arglist);
        SPUSH (lm, env_enter_frame);
        continue;
      }
    _funcall_lambda_cont:
      {
        typeof (s.uf_funcall_lambda_cont) *st = &s.uf_funcall_lambda_cont;

        Cell *params = st->fn->lambda.params;
        Cell *pairs
            = mapcar (KEYWORD (LIST), LIST2 (params, st->arglist, lm), lm);

        while (!IS_NIL (pairs))
          {
            Cell *pair = CAR (pairs);
            lm_env_let (lm, (CAR (pair))->symbol.str, CADR (pair));
            pairs = CDR (pairs);
          }

        SPUSH (lm, progn, NIL, st->fn->lambda.body);
        continue;
      }
    _list:
      {
        typeof (s.uf_list) *st = &s.uf_list;

        if (IS_NIL (st->arglist))
          {
            Cell *res = reverse_inplace (st->acc);
            PUSH (lm, res);
            continue;
          }
        else
          {
            Cell *car = CAR (st->arglist);
            Cell *cdr = CDR (st->arglist);

            SPUSH (lm, list_acc, st->acc, cdr);
            SPUSH (lm, eval, car);
          }
        continue;
      }
    _list_acc:
      {
        typeof (s.uf_list_acc) *st = &s.uf_list_acc;

        Cell *eval_res = POP (lm);
        Cell *acc = CONS (eval_res, st->acc, lm);
        SPUSH (lm, list, acc, st->arglist);
        continue;
      }
    _apply:
      {
        typeof (s.uf_apply) *st = &s.uf_apply;

        Cell *fn = st->fn ?: POP (lm);
        Cell *arglist = st->arglist ?: POP (lm);

        if (!LISTP (arglist))
          LM_ERR (ERR_MISSING_ARG, "APPLY");

        Cell *fixd_args = butlast (arglist, lm);

        SPUSH (lm, apply_cont, fn, arglist);
        SPUSH (lm, list, NIL, fixd_args);
        continue;
      }
    _apply_cont:
      {
        typeof (s.uf_apply_cont) *st = &s.uf_apply_cont;

        Cell *fixed_rev = POP (lm);
        Cell *tail_list = last (st->arglist, lm);

        if (!LISTP (tail_list)) // ie (apply fn NIL)
          SPUSH (lm, funcall, st->fn, NIL);
        else
          {
            SPUSH (lm, apply_funcall, st->fn, fixed_rev);
            SPUSH (lm, eval, tail_list);
          }
        continue;
      }
    _apply_funcall:
      {
        typeof (s.uf_apply_funcall) *st = &s.uf_apply_funcall;

        Cell *tail_list = POP (lm);

        if (!LISTP (tail_list))
          LM_ERR (ERR_MISSING_ARG, "FUNCALL");

        Cell *all_args = append_inplace (st->arglist, tail_list);
        SPUSH (lm, funcall, st->fn, all_args);
        continue;
      }
    _lispm:
      {
        typeof (s.uf_lispm) *st = &s.uf_lispm;

        if (st->fn == KEYWORD (LIST))
          SPUSH (lm, list, NIL, st->arglist);
        else if (st->fn == KEYWORD (FUNCALL))
          {
            SPUSH (lm, funcall, NULL, CDR (st->arglist));
            SPUSH (lm, eval, CAR (st->arglist));
          }
        else if (st->fn == KEYWORD (APPLY))
          {
            SPUSH (lm, apply, NULL, CDR (st->arglist));
            SPUSH (lm, eval, CAR (st->arglist));
          }
        else if (st->fn == KEYWORD (EVAL))
          {
            SPUSH (lm, eval, NULL);
            SPUSH (lm, eval, CAR (st->arglist));
          }
        else if (st->fn == KEYWORD (PROGN))
          SPUSH (lm, progn, NIL, st->arglist);
        else if (st->fn == KEYWORD (AND))
          SPUSH (lm, and, st->arglist);
        else if (st->fn == KEYWORD (OR))
          SPUSH (lm, or, st->arglist);
        else if (st->fn == KEYWORD (IF))
          SPUSH (lm, if, st->arglist);
        else
          LM_ERR (ERR_INTERNAL, "LISPM");
        continue;
      }
    _progn:
      {
        typeof (s.uf_progn) *st = &s.uf_progn;

        if (IS_NIL (st->arglist))
          PUSH (lm, st->res);
        else
          {
            SPUSH (lm, progn_cont, st->res, st->arglist);
            SPUSH (lm, eval, CAR (st->arglist));
          }
        continue;
      }
    _progn_cont:
      {
        typeof (s.uf_progn_cont) *st = &s.uf_progn_cont;

        Cell *new_res = POP (lm);
        SPUSH (lm, progn, new_res, CDR (st->arglist));
        continue;
      }
    _if:
      {
        typeof (s.uf_if) *st = &s.uf_if;

        Cell *pred_form = CAR (st->form);
        SPUSH (lm, if_cont, st->form);
        SPUSH (lm, eval, pred_form);
        continue;
      }

    _if_cont:
      {
        typeof (s.uf_if_cont) *st = &s.uf_if_cont;

        Cell *pred_val = POP (lm);

        if (!IS_NIL (pred_val))
          {
            Cell *then_form = CAR (CDR (st->form));
            SPUSH (lm, eval, then_form);
          }
        else
          {
            Cell *else_form = CAR (CDR (CDR (st->form)));
            if (else_form)
              SPUSH (lm, eval, else_form);
            else
              PUSH (lm, NIL);
          }
        continue;
      }
    _and:
      {
        typeof (s.uf_and) *st = &s.uf_and;

        if (IS_NIL (st->arglist))
          PUSH (lm, T);
        else
          {
            SPUSH (lm, and_cont, st->arglist);
            SPUSH (lm, eval, CAR (st->arglist));
          }
        continue;
      }
    _and_cont:
      {
        typeof (s.uf_and_cont) *st = &s.uf_and_cont;

        Cell *eval_res = POP (lm);

        if (IS_NIL (eval_res))
          {
            PUSH (lm, NIL);
            continue;
          }

        Cell *cdr = CDR (st->arglist);

        if (IS_NIL (cdr))
          PUSH (lm, eval_res);
        else
          {
            SPUSH (lm, and_cont, cdr);
            SPUSH (lm, eval, CAR (cdr));
          }
        continue;
      }
    _or:
      {
        typeof (s.uf_or) *st = &s.uf_or;

        if (IS_NIL (st->arglist))
          PUSH (lm, NIL);
        else
          {
            SPUSH (lm, or_cont, st->arglist);
            SPUSH (lm, eval, CAR (st->arglist));
          }
        continue;
      }
    _or_cont:
      {
        typeof (s.uf_or_cont) *st = &s.uf_or_cont;

        Cell *eval_res = POP (lm);

        if (!IS_NIL (eval_res))
          {
            PUSH (lm, eval_res);
            continue;
          }

        Cell *cdr = CDR (st->arglist);

        if (IS_NIL (cdr))
          PUSH (lm, eval_res);
        else
          {
            SPUSH (lm, or_cont, cdr);
            SPUSH (lm, eval, CAR (cdr));
          }
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
  SPUSH (lm, progn, NIL, progn);

  Cell *ret = lm_eval (lm);
  return ret;

overflow:
  lm_reset (lm);
  return NIL;
}
