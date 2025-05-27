#include "error.h"
#include "eval.h"

Node *
eval_eq (Node *args, Ctx *ctx)
{
  (void)ctx;
  Node *car = CAR (args);
  Node *card = CAR (CDR (args));

  EqFn fn = type (car)->eq_fn;

  if (fn (CAR (args), card))
    {
      return T;
    }

  return NIL;
}

Node *
eval_not (Node *args, Ctx *ctx)
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
