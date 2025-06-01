#include "lispm.h"

#include <stdarg.h>
#include <stdbool.h>

#include "debug.h"
#include "eval.h"
#include "lispm_utils.h"
#include "types.h"
#include "xalloc.h"

#define POP(lm) ((lm)->stk.cells[--(lm)->stk.sp])
#define PUSH(lm, ...)                                                         \
  do                                                                          \
    {                                                                         \
      if (!lipm_stk_push (lm, __VA_ARGS__, NULL))                             \
        goto overflow;                                                        \
    }                                                                         \
  while (0)

#define STATE_POP(lm) ((lm)->ctl.states[--(lm)->ctl.sp])
#define STATE_PUSH(__lm, __s)                                                 \
  do                                                                          \
    {                                                                         \
      if ((__lm)->ctl.sp >= LISPM_CTL_MAX)                                    \
        goto overflow;                                                        \
      (__lm)->ctl.states[(__lm)->ctl.sp++] = __s;                             \
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
      Cell *fn;
    } FUNCALL;
    struct
    {
      Cell *arglist;
      Cell *acc;
    } LIST;
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
} LMSecd;

static void
lm_secd_init (LMSecd *lm)
{
  lm->stk.sp = lm->ctl.sp = 0;
  lm->env = env_create ();
}

static void
lm_secd_destroy (LMSecd *lm)
{
  env_destroy (lm->env);
}

static inline bool
lipm_stk_push (LMSecd *lm, ...)
{
  va_list ap;
  Cell *val;

  va_start (ap, lm);
  while ((val = va_arg (ap, Cell *)))
    {
      if (lm->stk.sp >= LISPM_STK_MAX)
        {
          va_end (ap);
          return false;
        }
      lm->stk.cells[lm->stk.sp++] = val;
    }
  va_end (ap);
  return true;
}

static Cell *
lispm (LMSecd *lm, Context *ctx)
{
  char *err_msg;

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
        Cell *arg = s.EVAL.arg;

        if (IS (arg, SYMBOL))
          {
            PUSH (lm, lookup (arg, ctx));
            continue;
          }

        // literals, numbers, strings, etc.
        if (!LISTP (arg))
          {
            PUSH (lm, arg);
            continue;
          }

        // lists
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
              {
                PUSH (lm, CAR (cdr));
                continue;
              }

            if (IS (car, LAMBDA))
              {
                PUSH (lm, car);
                continue;
              }

            // __EVAL_CONT:
            STATE_PUSH (lm, ((State){ .state = EVAL_CONT, .EVAL.arg = cdr }));
            STATE_PUSH (lm, ((State){ .state = EVAL, .EVAL.arg = car }));
            continue;
          }
        goto error;
      }
    __EVAL_CONT:
      {
        Cell *fn = POP (lm);
        Cell *arglist = s.EVAL.arg;

        if (IS (fn, BUILTIN_FN) && fn->builtin_fn->sform)
          {
            // if (fn == KEYWORD (APPLY))
            //   return eval_apply (cdr, ctx);

            // if (fn == KEYWORD (FUNCALL))
            //   return eval_funcall (cdr, ctx);

            // if (fn == KEYWORD (EVAL))
            //   return eval (eval (CAR (cdr), ctx), ctx);

            // if (fn == KEYWORD (PROGN))
            //   return eval_progn (cdr, ctx);

            // if (fn == KEYWORD (AND))
            //   return and_form (cdr, ctx);

            // if (fn == KEYWORD (IF))
            //   return if_form (cdr, ctx);

            // if (fn == KEYWORD (OR))
            //   return or_form (cdr, ctx);

            return ERROR (ERR_INTERNAL, DEBUG_LOCATION, ctx);
          }

        STATE_PUSH (lm, ((State){
                            .state = FUNCALL,
                            .FUNCALL.fn = fn,
                        }));
        STATE_PUSH (lm, ((State){ .state = LIST,
                                  .LIST.acc = NIL,
                                  .LIST.arglist = arglist }));
        continue;
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

        STATE_PUSH (lm, ((State){ .state = LIST_ACC,
                                  .LIST.arglist = cdr,
                                  .LIST.acc = rev }));

        STATE_PUSH (lm, ((State){ .state = EVAL, .EVAL.arg = car }));
        continue;
      }

    __LIST_ACC:
      {
        Cell *eval_res = POP (lm);

        Cell *old_rev = s.LIST.acc;
        Cell *rev2 = CONS (eval_res, old_rev, ctx);

        STATE_PUSH (lm, ((State){ .state = LIST,
                                  .LIST.arglist = s.LIST.arglist,
                                  .LIST.acc = rev2 }));
        continue;
      }

    __PROGN:
      {
      }
    __APPLY:
      {
      }
    __FUNCALL:
      {
        if (IS (s.FUNCALL.fn, BUILTIN_FN))
          STATE_PUSH (lm, ((State){ .state = FUNCALL_BUILTIN,
                                    .FUNCALL.fn = s.FUNCALL.fn }));

        if (IS (s.FUNCALL.fn, LAMBDA))
          STATE_PUSH (lm, ((State){ .state = FUNCALL_LAMBDA,
                                    .FUNCALL.fn = s.FUNCALL.fn }));

        goto error;
      }
    __FUNCALL_BUILTIN:
      {
        Cell *fn = s.FUNCALL.fn;

        Cell *arglist = POP (lm);

        int received = (int)length (arglist);
        const BuiltinFn *builtin_fn = fn->builtin_fn;

        if (builtin_fn->arity > 0 && builtin_fn->arity != received)
          {
            ErrorCode err = (received < builtin_fn->arity)
                                ? ERR_MISSING_ARG
                                : ERR_UNEXPECTED_ARG;
            return ERROR (err, builtin_fn->name, ctx);
          }
        // eval_apply or eval_funcall could have taken us here.
        // so if we called them again, arglist would be eval'd 2x.
        if (fn == KEYWORD (FUNCALL))
          {
            Cell *fn2 = eval (CAR (arglist), ctx);
            return funcall (fn2, CDR (arglist), ctx);
          }

        if (fn == KEYWORD (APPLY))
          {
            Cell *fn2 = eval (CAR (arglist), ctx);
            return funcall (fn2, CAR (CDR (arglist)), ctx);
          }

        if (fn == KEYWORD (LIST))
          return arglist; // LIST is eval_list, so we're done.

        return builtin_fn->fn (arglist, ctx);
      }
    __FUNCALL_LAMBDA:
      {
        Cell *arglist = POP (lm);
      }
    }

  Cell *evl_ret = POP (lm);
  return evl_ret;

error:;
  perror (err_msg);
  return NIL;

overflow:;
  perror ("**Overflow");
  return NIL;
}

Cell *
lispm_progn (Cell *progn, Context *ctx)
{
  LMSecd lm;
  lm_secd_init (&lm);

  Cell *rev_progn = reverse (progn, ctx);

  for (Cell *c = rev_progn; !IS_NIL (c); c = CDR (c))
    STATE_PUSH (&lm, ((State){ .state = EVAL, .EVAL.arg = CAR (c) }));

  Cell *ret = lispm (&lm, ctx);
  lm_secd_destroy (&lm);
  return ret;

overflow:
  lm_secd_destroy (&lm);
  perror ("Reset");
  return NIL;
}
