#include "error.h"
#include "eval.h"

Node *
eval_gt (Node *args, Ctx *ctx)
{
  (void)ctx;
  Node *result = T;

  if (!IS_CONS (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "gt: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "gt: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (CAR (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "gt: argument is not integer");
      return NULL;
    }

  Node *prev = CAR (args);

  for (Node *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INTEGER (CAR (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "gt: argument is not integer");
          return NULL;
        }

      if (!(GET_INTEGER (prev) > GET_INTEGER (CAR (rest))))
        {
          return NIL;
        }

      prev = CAR (rest);
    }

  return result;
}

Node *
eval_lt (Node *args, Ctx *ctx)
{
  (void)ctx;
  Node *result = T;

  if (!IS_CONS (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "lt: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "lt: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (CAR (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "lt: argument is not integer");
      return NULL;
    }

  Node *prev = CAR (args);

  for (Node *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INTEGER (CAR (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "lt: argument is not integer");
          return NULL;
        }

      if (!(GET_INTEGER (prev) < GET_INTEGER (CAR (rest))))
        {
          return NIL;
        }

      prev = CAR (rest);
    }

  return result;
}

// TODO: BUG IN gt (1 OR more args) and add, and mul take  (0 or more)
// TODO: check for over/underflow someday

Node *
eval_add (Node *args, Ctx *ctx)
{
  if (!IS_CONS (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "add: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "add: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (CAR (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "add: argument is not integer");
      return NULL;
    }

  Integer sum = GET_INTEGER (CAR (args));

  for (Node *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INTEGER (CAR (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "add: argument is not integer");
          return NULL;
        }

      sum += GET_INTEGER (CAR (rest));
    }

  return cons_integer (sum, ctx);
}

Node *
eval_sub (Node *args, Ctx *ctx)
{
  if (!IS_CONS (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "sub: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "sub: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (CAR (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "sub: argument is not integer");
      return NULL;
    }

  Integer total = GET_INTEGER (CAR (args));

  for (Node *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INTEGER (CAR (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "sub: argument is not integer");
          return NULL;
        }

      total -= GET_INTEGER (CAR (rest));
    }

  return cons_integer (total, ctx);
}

Node *
eval_mul (Node *args, Ctx *ctx)
{
  if (!IS_CONS (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "mul: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "mul: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (CAR (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "mul: argument is not integer");
      return NULL;
    }

  Integer result = GET_INTEGER (CAR (args));

  for (Node *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INTEGER (CAR (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "mul: argument is not integer");
          return NULL;
        }

      result *= GET_INTEGER (CAR (rest));
    }

  return cons_integer (result, ctx);
}

Node *
eval_div (Node *args, Ctx *ctx)
{
  if (!IS_CONS (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "div: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "div: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (CAR (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "div: argument is not integer");
      return NULL;
    }

  Integer result = GET_INTEGER (CAR (args));

  for (Node *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INTEGER (CAR (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "div: argument is not integer");
          return NULL;
        }

      if (GET_INTEGER (CAR (rest)) == 0)
        {
          raise (ERR_DIVISION_BY_0, "div: argument is zero");
          return NULL;
        }

      result /= GET_INTEGER (CAR (rest));
    }

  return cons_integer (result, ctx);
}
