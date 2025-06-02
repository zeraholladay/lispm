#include "error.h"
#include "eval.h"

Cell *
eval_eq (Cell *args, LM *lm)
{
  (void)lm;
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
eval_not (Cell *args, LM *lm)
{
  (void)lm;
  EqFn nil_eq = type (NIL)->eq;

  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "not", lm);

  return nil_eq (NIL, CAR (args)) ? T : NIL;
}
