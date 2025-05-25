#include "error.h"
#include "eval.h"

Node *
eval_gt (Node *args, Context *ctx)
{
  (void)ctx;
  Node *result = T;

  if (!IS_LIST (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "gt: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "gt: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (FIRST (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "gt: argument is not integer");
      return NULL;
    }

  Node *prev = FIRST (args);

  for (Node *rest = REST (args); !IS_NIL (rest); rest = REST (rest))
    {
      if (!IS_INTEGER (FIRST (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "gt: argument is not integer");
          return NULL;
        }

      if (!(GET_INTEGER (prev) > GET_INTEGER (FIRST (rest))))
        {
          return NIL;
        }

      prev = FIRST (rest);
    }

  return result;
}

Node *
eval_lt (Node *args, Context *ctx)
{
  (void)ctx;
  Node *result = T;

  if (!IS_LIST (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "lt: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "lt: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (FIRST (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "lt: argument is not integer");
      return NULL;
    }

  Node *prev = FIRST (args);

  for (Node *rest = REST (args); !IS_NIL (rest); rest = REST (rest))
    {
      if (!IS_INTEGER (FIRST (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "lt: argument is not integer");
          return NULL;
        }

      if (!(GET_INTEGER (prev) < GET_INTEGER (FIRST (rest))))
        {
          return NIL;
        }

      prev = FIRST (rest);
    }

  return result;
}

// TODO: BUG IN gt (1 OR more args) and add, and mul take  (0 or more)
// TODO: check for over/underflow someday

Node *
eval_add (Node *args, Context *ctx)
{
  if (!IS_LIST (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "add: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "add: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (FIRST (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "add: argument is not integer");
      return NULL;
    }

  Integer sum = GET_INTEGER (FIRST (args));

  for (Node *rest = REST (args); !IS_NIL (rest); rest = REST (rest))
    {
      if (!IS_INTEGER (FIRST (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "add: argument is not integer");
          return NULL;
        }

      sum += GET_INTEGER (FIRST (rest));
    }

  return cons_integer (&CTX_POOL (ctx), sum);
}

Node *
eval_sub (Node *args, Context *ctx)
{
  if (!IS_LIST (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "sub: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "sub: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (FIRST (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "sub: argument is not integer");
      return NULL;
    }

  Integer total = GET_INTEGER (FIRST (args));

  for (Node *rest = REST (args); !IS_NIL (rest); rest = REST (rest))
    {
      if (!IS_INTEGER (FIRST (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "sub: argument is not integer");
          return NULL;
        }

      total -= GET_INTEGER (FIRST (rest));
    }

  return cons_integer (&CTX_POOL (ctx), total);
}

Node *
eval_mul (Node *args, Context *ctx)
{
  if (!IS_LIST (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "mul: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "mul: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (FIRST (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "mul: argument is not integer");
      return NULL;
    }

  Integer result = GET_INTEGER (FIRST (args));

  for (Node *rest = REST (args); !IS_NIL (rest); rest = REST (rest))
    {
      if (!IS_INTEGER (FIRST (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "mul: argument is not integer");
          return NULL;
        }

      result *= GET_INTEGER (FIRST (rest));
    }

  return cons_integer (&CTX_POOL (ctx), result);
}

Node *
eval_div (Node *args, Context *ctx)
{
  if (!IS_LIST (args))
    {
      raise (ERR_ARG_NOT_ITERABLE, "div: argument is not a list");
      return NULL;
    }

  if (IS_NIL (args))
    {
      raise (ERR_INVALID_ARG_LENGTH, "div: expected >= 1 arguments");
      return NULL;
    }

  if (!IS_INTEGER (FIRST (args)))
    {
      raise (ERR_ARG_TYPE_MISMATCH, "div: argument is not integer");
      return NULL;
    }

  Integer result = GET_INTEGER (FIRST (args));

  for (Node *rest = REST (args); !IS_NIL (rest); rest = REST (rest))
    {
      if (!IS_INTEGER (FIRST (rest)))
        {
          raise (ERR_ARG_TYPE_MISMATCH, "div: argument is not integer");
          return NULL;
        }

      if (GET_INTEGER (FIRST (rest)) == 0)
        {
          raise (ERR_DIVISION_BY_0, "div: argument is zero");
          return NULL;
        }

      result /= GET_INTEGER (FIRST (rest));
    }

  return cons_integer (&CTX_POOL (ctx), result);
}
