#include "lispm.h"

#include "debug.h"
#include "eval.h"
#include "lispm_utils.h"
#include "types.h"
#include "xalloc.h"

#define POP_STK(lm) ((lm)->stk.cells[--(lm)->stk.sp])
#define PUSH_STK(lm, val)                                                     \
  do                                                                          \
    {                                                                         \
      if ((lm)->stk.sp >= LISPM_STK_MAX)                                      \
        goto reset;                                                           \
      (lm)->stk.cells[(lm)->stk.sp++] = (val);                                \
    }                                                                         \
  while (0)

#define POP_CTL(lm) ((lm)->ctl.states[--(lm)->ctl.sp])
#define PUSH_CTL(lm, state)                                                   \
  do                                                                          \
    {                                                                         \
      if ((lm)->ctl.sp >= LISPM_STK_MAX)                                      \
        goto reset;                                                           \
      (lm)->ctl.states[(lm)->ctl.sp++] = state;                               \
    }                                                                         \
  while (0)

typedef enum
{
  STATE_EVAL,
  STATE_EVAL_FN,
  STATE_EVAL_LIST,
  STATE_EVAL_PROGN,
  STATE_EVAL_APPLY,
  STATE_EVAL_FUNCALL,
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
    State states[LISPM_STK_MAX];
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

Cell *
lispm_eval (LMSecd *lm, Context *ctx)
{
  char *err_msg;

  while (lm->ctl.sp)
    {
      State s = POP_CTL (lm);

      switch (s)
        {
        case STATE_EVAL:
          {
            Cell *form = POP_STK (lm);

            if (IS (form, SYMBOL))
              {
                PUSH_STK (lm, lookup (form, ctx));
                goto next;
              }

            // literals, numbers, strings, etc.
            if (!LISTP (form))
              {
                PUSH_STK (lm, form);
                goto next;
              }

            // list cars
            if (LISTP (form))
              {
                if (IS_NIL (form))
                  {
                    PUSH_STK (lm, NIL);
                    goto next;
                  }

                Cell *car = CAR (form);
                Cell *cdr = CDR (form);

                if (car == KEYWORD (QUOTE))
                  {
                    PUSH_STK (lm, CAR (cdr));
                    goto next;
                  }

                if (IS (car, LAMBDA))
                  {
                    PUSH_STK (lm, car);
                    goto next;
                  }

                PUSH_STK (lm, cdr); // save the cdr
                PUSH_CTL (lm, STATE_EVAL_FN);

                PUSH_STK (lm, car); // ie the function we need evaluated
                PUSH_CTL (lm, STATE_EVAL);
                goto next;
              }
            goto error;
          }
        case STATE_EVAL_FN:
          {
            Cell *fn = POP_STK (lm);
            Cell *arglist = POP_STK (lm);

            // sforms
            if (IS (fn, BUILTIN_FN) && fn->builtin_fn->sform)
              {
                if (fn == KEYWORD (APPLY))
                  {
                    PUSH_STK (lm, arglist);
                    PUSH_CTL (lm, STATE_EVAL_APPLY);
                    goto next;
                  }

                if (fn == KEYWORD (FUNCALL))
                  {
                    Cell *fn2 = CAR (arglist);
                    Cell *arglist2 = CDR (arglist);
                    PUSH_STK (lm, fn2);
                    PUSH_CTL (lm, STATE_EVAL_FUNCALL);
                    PUSH_STK (lm, arglist2);
                    PUSH_CTL (lm, STATE_EVAL_LIST);
                    goto next;
                  }

                if (fn == KEYWORD (EVAL))
                  {
                    PUSH_STK (lm, CAR (arglist));
                    PUSH_CTL (lm, STATE_EVAL);
                    PUSH_CTL (lm, STATE_EVAL);
                    goto next;
                  }

                if (fn == KEYWORD (PROGN))
                  {
                    PUSH_STK (lm, arglist);
                    PUSH_CTL (lm, STATE_EVAL_PROGN);
                    goto next;
                  }
              }

            PUSH_STK (lm, fn);
            PUSH_CTL (lm, STATE_EVAL_FUNCALL);

            PUSH_STK (lm, arglist);
            PUSH_CTL (lm, STATE_EVAL_LIST);
            goto next;
          }
          break;

        case STATE_EVAL_LIST:
          {
            Cell *arglist = POP_STK (lm);
            Cell *rev = NIL;

            return reverse_inplace (rev);
            rev = CONS (eval_res, rev, ctx);

            Cons *rev_arglist = reverse (arglist, ctx);

            for (Cell *c = arglist; !IS_NIL (c); c = CDR (c))
              {
                PUSH_STK (lm, CAR (c));
                PUSH_CTL (lm, STATE_EVAL);
              }

            goto next;
          }
          break;

        case STATE_EVAL_FUNCALL:
          {
            Cell *fn = POP_STK (lm);
            Cell *arglist = POP_STK (lm);

            if (IS (fn, BUILTIN_FN))
              {
                int received = (int)length (arglist);
                const BuiltinFn *builtin_fn = fn->builtin_fn;

                if (builtin_fn->arity > 0 && builtin_fn->arity != received)
                  {
                    ErrorCode err = (received < builtin_fn->arity)
                                        ? ERR_MISSING_ARG
                                        : ERR_UNEXPECTED_ARG;
                    return ERROR (err, builtin_fn->name, ctx);
                  }
                // // eval_apply or eval_funcall could have taken us here.
                // // so if we called them again, arglist would be eval'd 2x.
                // if (fn == KEYWORD (FUNCALL))
                //   {
                //     Cell *fn2 = eval (CAR (arglist), ctx);
                //     return funcall (fn2, CDR (arglist), ctx);
                //   }

                // if (fn == KEYWORD (APPLY))
                //   {
                //     Cell *fn2 = eval (CAR (arglist), ctx);
                //     return funcall (fn2, CAR (CDR (arglist)), ctx);
                //   }

                // if (fn == KEYWORD (LIST))
                //   return arglist; // LIST is eval_list, so we're done.

                return builtin_fn->fn (arglist, ctx);
              }

            if (IS (fn, LAMBDA))
              {
                return funcall_lambda (fn, arglist, ctx);
              }

            goto error;
          }
          break;
        default:
          goto error;
          break;
        }
    next:;
    }

  Cell *evl_ret = POP_STK (lm);
  return evl_ret;

error:;
  perror ("Error");
  return NIL;

reset:
  perror ("Reset");
  return NIL;
}

Cell *
lispm_progn (Cell *progn, Context *ctx)
{
  LMSecd lm;
  lm_secd_init (&lm);

  Cell *rev_progn = reverse (progn, ctx);

  for (Cell *c = rev_progn; !IS_NIL (c); c = CDR (c))
    {
      PUSH_STK (&lm, CAR (c));
      PUSH_CTL (&lm, STATE_EVAL);
    }

  Cell *ret = lispm_eval (&lm, ctx);
  lm_secd_destroy (&lm);
  return ret;

reset:
  lm_secd_destroy (&lm);
  perror ("Reset");
  return NIL;
}
