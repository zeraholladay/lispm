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
eval_and (Node *args, Context *ctx)
{
  Node *eval_result = T;
  EqFn nil_eq_fn = type (NIL)->eq_fn;

  while (!IS_NIL (args))
    {
      eval_result = eval (FIRST (args), ctx);

      if (nil_eq_fn (NIL, eval_result))
        {
          return NIL;
        }

      args = REST (args);
    }

  return eval_result;
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

Node *
eval_or (Node *args, Context *ctx)
{
  Node *eval_result = NIL;
  EqFn nil_eq_fn = type (NIL)->eq_fn;

  while (!IS_NIL (args))
    {
      eval_result = eval (FIRST (args), ctx);

      if (!nil_eq_fn (NIL, eval_result))
        {
          return eval_result;
        }

      args = REST (args);
    }

  return NIL;
}
