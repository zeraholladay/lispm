#include "error.h"
#include "eval.h"

Cell *
eval_eq (Cell *args, Context *ctx)
{
  (void)ctx;
  Cell *car = CAR (args);
  Cell *card = CAR (CDR (args));

  EqFn fn = type (car)->eq;

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
  EqFn nil_eq = type (NIL)->eq;

  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "not", ctx);

  return nil_eq (NIL, CAR (args)) ? T : NIL;
}
