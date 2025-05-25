#include "error.h"
#include "eval.h"

Node *
eval_eq (Node *args, Context *ctx)
{
  (void)ctx;
  Node *first = FIRST (args);
  Node *second = FIRST (REST (args));

  EqFn fn = type (first)->eq_fn;

  if (fn (FIRST (args), second))
    {
      return T;
    }

  return NIL;
}

Node *
eval_not (Node *args, Context *ctx)
{
  (void)ctx;
  EqFn nil_eq_fn = type (NIL)->eq_fn;

  if (!LISTP (args))
    {
      raise (ERR_INVALID_ARG, "not");
      return NULL;
    }

  return nil_eq_fn (NIL, FIRST (args)) ? T : NIL;
}
