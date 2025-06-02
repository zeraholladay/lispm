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

#define STATE_PUSH(lm, ...)                                                   \
  do                                                                          \
    {                                                                         \
      if (((lm)->ctl.sp) >= LISPM_CTL_MAX)                                    \
        goto overflow;                                                        \
      (lm)->ctl.states[(lm)->ctl.sp++] = (State){ __VA_ARGS__ };              \
    }                                                                         \
  while (0)

typedef enum
{
#define STATE(name) name,
#include "lispm_states.def"
#undef STATE
  COUNT,
} StateEnum;

typedef struct state
{
  StateEnum state;
  union
  {
    struct
    {
      Cell *arg;
    } EVAL;
    struct
    {
      Cell *fn, *arglist;
    } FUNCALL, APPLY, LISPM;
    struct lispm
    {
      Cell *res, *arglist;
    } PROGN, AND, OR;
    struct
    {
      Cell *acc, *arglist;
    } LIST;
    struct
    {
      Cell *form;
    } IF;
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

static Cell *
lm_eval (LM *lm)
{
  while (lm->ctl.sp)
    {
      State s = STATE_POP (lm);
      switch (s.state)
        {
#define STATE(name)                                                           \
  case name:                                                                  \
    goto __##name;
#include "lispm_states.def"
#undef STATE
        default:
          goto error;
        }
    __EVAL:
      {
        Cell *arg = (s.EVAL.arg) ? s.EVAL.arg : POP (lm);

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
                STATE_PUSH (lm, .state = EVAL_CONT, .EVAL.arg = cdr);
                STATE_PUSH (lm, .state = EVAL, .EVAL.arg = car);
              }
            continue;
          }
        goto error;
      }
    __EVAL_CONT:
      {
        Cell *fn = POP (lm);
        Cell *arglist = s.EVAL.arg;

        // one of the fns this C code handles
        if (IS (fn, BUILTIN_FN) && fn->builtin_fn->is_lispm)
          {
            STATE_PUSH (lm, .state = LISPM, .LISPM.fn = fn,
                        .LISPM.arglist = arglist);
            continue;
          }

        STATE_PUSH (lm, .state = FUNCALL, .FUNCALL.fn = fn, );
        STATE_PUSH (lm, .state = LIST, .LIST.acc = NIL,
                    .LIST.arglist = arglist);
        continue;
      }
    __FUNCALL:
      {
        Cell *arglist = (s.FUNCALL.arglist) ? (s.FUNCALL.arglist) : POP (lm);
        Cell *fn = (s.FUNCALL.fn) ? (s.FUNCALL.fn) : POP (lm);

        if (IS (fn, BUILTIN_FN))
          STATE_PUSH (lm, .state = FUNCALL_BUILTIN, .FUNCALL.fn = fn,
                      .FUNCALL.arglist = arglist);
        else if (IS (fn, LAMBDA))
          STATE_PUSH (lm, .state = FUNCALL_LAMBDA, .FUNCALL.fn = fn,
                      .FUNCALL.arglist = arglist);
        else
          goto error;

        continue;
      }
    __FUNCALL_BUILTIN:
      {
        Cell *fn = s.FUNCALL.fn;
        Cell *arglist = s.FUNCALL.arglist;

        int received = (int)length (arglist);
        const BuiltinFn *builtin_fn = fn->builtin_fn;

        if (builtin_fn->arity > 0 && builtin_fn->arity != received)
          {
            ErrorCode err = (received < builtin_fn->arity)
                                ? ERR_MISSING_ARG
                                : ERR_UNEXPECTED_ARG;
            goto error; // return ERROR (err, builtin_fn->name, lm);
          }

        if (!builtin_fn->fn)
          {
            goto error;
          }

        Cell *res = builtin_fn->fn (arglist, lm);
        PUSH (lm, res);
        continue;
      }
    __FUNCALL_LAMBDA:
      {
        // Cell *fn = s.FUNCALL.fn;
        // Cell *arglist = s.FUNCALL.arglist;
      }
    __LIST:
      {
        Cell *arglist = s.LIST.arglist;
        Cell *rev = s.LIST.acc;

        if (IS_NIL (arglist))
          {
            Cell *res = reverse_inplace (rev);
            PUSH (lm, res);
            continue;
          }

        Cell *car = CAR (arglist);
        Cell *cdr = CDR (arglist);

        STATE_PUSH (lm, .state = LIST_ACC, .LIST.arglist = cdr,
                    .LIST.acc = rev);
        STATE_PUSH (lm, .state = EVAL, .EVAL.arg = car);
        continue;
      }
    __LIST_ACC:
      {
        Cell *eval_res = POP (lm);

        Cell *old_rev = s.LIST.acc;
        Cell *rev2 = CONS (eval_res, old_rev, lm);

        STATE_PUSH (lm, .state = LIST, .LIST.arglist = s.LIST.arglist,
                    .LIST.acc = rev2);
        continue;
      }
    __APPLY:
      {
        Cell *arglist = s.APPLY.arglist ? s.APPLY.arglist : POP (lm);
        Cell *fn = s.APPLY.fn ? s.APPLY.fn : POP (lm);

        if (!LISTP (arglist))
          goto error;

        Cell *fixd_args = butlast (arglist, lm);

        STATE_PUSH (lm, .state = APPLY_CONT, .APPLY.fn = fn,
                    .APPLY.arglist = arglist);
        STATE_PUSH (lm, .state = LIST, .LIST.arglist = fixd_args,
                    .LIST.acc = NIL);
        continue;
      }
    __APPLY_CONT:
      {
        Cell *fixed_rev = POP (lm);
        Cell *arglist = s.APPLY.arglist;
        Cell *tail_list = last (arglist, lm);

        if (!LISTP (tail_list)) // ie (apply fn NIL)
          {
            STATE_PUSH (lm, .state = FUNCALL, .FUNCALL.fn = s.APPLY.fn,
                        .FUNCALL.arglist = NIL);
            continue;
          }

        STATE_PUSH (lm, .state = APPLY_FUNCALL, .APPLY.fn = s.APPLY.fn,
                    .APPLY.arglist = fixed_rev);
        STATE_PUSH (lm, .state = EVAL, .EVAL.arg = tail_list);
        continue;
      }
    __APPLY_FUNCALL:
      {
        Cell *tail_list = POP (lm);

        if (!LISTP (tail_list))
          goto error;

        Cell *fixed_rev = s.APPLY.arglist; // the reversed list of fixed‐values
        Cell *fn = s.APPLY.fn;

        Cell *all_args = append_inplace (fixed_rev, tail_list);

        STATE_PUSH (lm, .state = FUNCALL, .FUNCALL.fn = fn,
                    .FUNCALL.arglist = all_args);
        continue;
      }
    __LISPM:
      {
        Cell *fn = s.FUNCALL.fn;
        Cell *arglist = s.FUNCALL.arglist;

        if (fn == KEYWORD (LIST))
          STATE_PUSH (lm, .state = LIST, .LIST.acc = NIL,
                      .LIST.arglist = arglist);
        else if (fn == KEYWORD (FUNCALL))
          {
            STATE_PUSH (lm, .state = FUNCALL,
                        .FUNCALL.arglist = CDR (arglist));
            STATE_PUSH (lm, .state = EVAL, .EVAL.arg = CAR (arglist));
          }
        else if (fn == KEYWORD (APPLY))
          {
            STATE_PUSH (lm, .state = APPLY,
                        .APPLY.arglist = CAR (CDR (arglist)));
            STATE_PUSH (lm, .state = EVAL, .EVAL.arg = CAR (arglist));
          }
        else if (fn == KEYWORD (EVAL))
          {
            STATE_PUSH (lm, .state = EVAL, .EVAL.arg = CAR (arglist));
            STATE_PUSH (lm, .state = EVAL);
          }
        else if (fn == KEYWORD (PROGN))
          STATE_PUSH (lm, .state = PROGN, .PROGN.arglist = arglist);
        else if (fn == KEYWORD (AND))
          STATE_PUSH (lm, .state = AND, .AND.arglist = arglist);
        else if (fn == KEYWORD (OR))
          STATE_PUSH (lm, .state = OR, .OR.arglist = arglist);
        else if (fn == KEYWORD (IF))
          STATE_PUSH (lm, .state = IF, .IF.form = arglist);
        else
          {
            goto error;
          }
        continue;
      }
    __PROGN:
      {
        Cell *res = (s.PROGN.res) ? (s.PROGN.res) : NIL;
        Cell *arglist = s.PROGN.arglist;

        if (IS_NIL (arglist))
          {
            PUSH (lm, res);
            continue;
          }

        STATE_PUSH (lm, .state = PROGN_CONT, .PROGN.res = res,
                    .PROGN.arglist = arglist);
        STATE_PUSH (lm, .state = EVAL, .EVAL.arg = CAR (arglist));
        continue;
      }
    __PROGN_CONT:
      {
        Cell *arglist = s.PROGN.arglist;
        Cell *new_res = POP (lm);

        STATE_PUSH (lm, .state = PROGN, .PROGN.res = new_res,
                    .PROGN.arglist = CDR (arglist));
        continue;
      }
    __IF:
      {
        Cell *form = s.IF.form;
        Cell *pred_form = CAR (form);

        STATE_PUSH (lm, .state = IF_CONT, .IF.form = form);
        STATE_PUSH (lm, .state = EVAL, .EVAL.arg = pred_form);
        continue;
      }

    __IF_CONT:
      {
        Cell *form = s.IF.form;
        Cell *pred_val = POP (lm);

        if (!IS_NIL (pred_val))
          {
            Cell *then_form = CAR (CDR (form));
            STATE_PUSH (lm, .state = EVAL, .EVAL.arg = then_form);
          }
        else
          {
            Cell *else_form = CAR (CDR (CDR (form)));
            if (else_form)
              STATE_PUSH (lm, .state = EVAL, .EVAL.arg = else_form);
            else
              PUSH (lm, NIL);
          }
        continue;
      }
    __AND:
      {
        Cell *arglist = s.AND.arglist;

        if (IS_NIL (arglist))
          {
            PUSH (lm, T);
            continue;
          }

        STATE_PUSH (lm, .state = AND_CONT, .AND.arglist = arglist);
        STATE_PUSH (lm, .state = EVAL, .EVAL.arg = CAR (arglist));
        continue;
      }
    __AND_CONT:
      {
        Cell *arglist = s.AND.arglist;
        Cell *eval_res = POP (lm);

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

        STATE_PUSH (lm, .state = AND_CONT, .AND.res = eval_res,
                    .AND.arglist = cdr);

        STATE_PUSH (lm, .state = EVAL, .EVAL.arg = CAR (cdr));
        continue;
      }
    __OR:
      {
        Cell *arglist = s.OR.arglist;

        if (IS_NIL (arglist))
          {
            PUSH (lm, NIL);
            continue;
          }

        STATE_PUSH (lm, .state = OR_CONT, .OR.arglist = arglist);
        STATE_PUSH (lm, .state = EVAL, .EVAL.arg = CAR (arglist));
        continue;
      }
    __OR_CONT:
      {
        Cell *arglist = s.OR.arglist;
        Cell *eval_res = POP (lm);

        if (IS_NIL (eval_res))
          {
            PUSH (lm, T);
            continue;
          }

        Cell *cdr = CDR (arglist);

        if (!IS_NIL (cdr))
          {
            PUSH (lm, eval_res);
            continue;
          }

        STATE_PUSH (lm, .state = OR_CONT, .OR.res = eval_res,
                    .OR.arglist = cdr);
        STATE_PUSH (lm, .state = EVAL, .EVAL.arg = CAR (cdr));
        continue;
      }
    }

  Cell *evl_ret = POP (lm);
  return evl_ret;

error:;
  perror ("**Error:");
  return NIL;

underflow:;
  perror ("**Underflow:");
  return NIL;

overflow:;
  perror ("**Overflow");
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
  lm = lm_create ();

  STATE_PUSH (lm, .state = PROGN, .PROGN.arglist = progn);

  Cell *ret = lm_eval (lm);
  lm_destroy (lm);
  return ret;

overflow:
  lm_destroy (lm);
  perror ("Reset");
  return NIL;
}
