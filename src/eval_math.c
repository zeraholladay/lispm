#include "error.h"
#include "eval.h"

Cell *
eval_gt (Cell *args, Context *ctx)
{
  (void)ctx;
  Cell *result = T;

  if (!IS (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "gt: argument is not a list", ctx);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "gt: expected >= 1 arguments", ctx);

  if (!IS (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "gt: argument is not integer", ctx);

  Cell *prev = CAR (args);

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "gt: argument is not integer",
                      ctx);

      if (!(prev->integer > CAR (rest)->integer))
        return NIL;

      prev = CAR (rest);
    }

  return result;
}

Cell *
eval_lt (Cell *args, Context *ctx)
{
  (void)ctx;
  Cell *result = T;

  if (!IS (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "lt: argument is not a list", ctx);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "lt: expected >= 1 arguments", ctx);

  if (!IS (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "lt: argument is not integer", ctx);

  Cell *prev = CAR (args);

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "lt: argument is not integer",
                      ctx);

      if (!(prev->integer < CAR (rest)->integer))
        return NIL;

      prev = CAR (rest);
    }

  return result;
}

// TODO: BUG IN gt (1 OR more args) and add, and mul take  (0 or more)
// TODO: check for over/underflow someday

Cell *
eval_add (Cell *args, Context *ctx)
{
  if (!IS (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "add: argument is not a list", ctx);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "add: expected >= 1 arguments", ctx);

  if (!IS (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "add: argument is not integer", ctx);

  Integer sum = CAR (args)->integer;

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "add: argument is not integer",
                      ctx);

      sum += CAR (rest)->integer;
    }

  return INTEGER (sum, ctx);
}

Cell *
eval_sub (Cell *args, Context *ctx)
{
  if (!IS (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "sub: argument is not a list", ctx);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "sub: expected >= 1 arguments", ctx);

  if (!IS (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "sub: argument is not integer", ctx);

  Integer total = CAR (args)->integer;

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "sub: argument is not integer",
                      ctx);

      total -= CAR (rest)->integer;
    }

  return INTEGER (total, ctx);
}

Cell *
eval_mul (Cell *args, Context *ctx)
{
  if (!IS (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "mul: argument is not a list", ctx);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "mul: expected >= 1 arguments", ctx);

  if (!IS (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "mul: argument is not integer", ctx);

  Integer result = CAR (args)->integer;

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "mul: argument is not integer",
                      ctx);

      result *= CAR (rest)->integer;
    }

  return INTEGER (result, ctx);
}

Cell *
eval_div (Cell *args, Context *ctx)
{
  if (!IS (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "div: argument is not a list", ctx);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "div: expected >= 1 arguments", ctx);

  if (!IS (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "div: argument is not integer", ctx);

  Integer result = CAR (args)->integer;

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "div: argument is not integer",
                      ctx);

      if (CAR (rest)->integer == 0)
        return ERROR (ERR_DIVISION_BY_0, "div: argument is zero", ctx);

      result /= CAR (rest)->integer;
    }

  return INTEGER (result, ctx);
}
