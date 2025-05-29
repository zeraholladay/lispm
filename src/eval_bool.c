#include "error.h"
#include "eval.h"

Cell *
eval_eq (Cell *args, Context *ctx)
{
  (void)ctx;
  Cell *car = CAR (args);
  Cell *card = CAR (CDR (args));

  EqFn fn = type (car)->eq_fn;

  if (fn (CAR (args), card))
    {
      return T;
    }

  return NIL;
}

Cell *
eval_not (Cell *args, Context *ctx)
{
  (void)ctx;
  EqFn nil_eq_fn = type (NIL)->eq_fn;

  if (!LISTP (args))
    {
      raise (ERR_INVALID_ARG, "not");
      return NULL;
    }

  return nil_eq_fn (NIL, CAR (args)) ? T : NIL;
}
