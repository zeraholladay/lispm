#include "error.h"
#include "eval.h"

Cell *
eval_gt (Cell *args, LM *lm)
{
  (void)lm;
  Cell *result = T;

  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "gt: argument is not a list", lm);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "gt: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "gt: argument is not integer", lm);

  Cell *prev = CAR (args);

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "gt: argument is not integer",
                      lm);

      if (!(prev->integer > CAR (rest)->integer))
        return NIL;

      prev = CAR (rest);
    }

  return result;
}

Cell *
eval_lt (Cell *args, LM *lm)
{
  (void)lm;
  Cell *result = T;

  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "lt: argument is not a list", lm);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "lt: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "lt: argument is not integer", lm);

  Cell *prev = CAR (args);

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "lt: argument is not integer",
                      lm);

      if (!(prev->integer < CAR (rest)->integer))
        return NIL;

      prev = CAR (rest);
    }

  return result;
}

// TODO: BUG IN gt (1 OR more args) and add, and mul take  (0 or more)
// TODO: check for over/underflow someday

Cell *
eval_add (Cell *args, LM *lm)
{
  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "add: argument is not a list", lm);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "add: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "add: argument is not integer", lm);

  Integer sum = CAR (args)->integer;

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "add: argument is not integer",
                      lm);

      sum += CAR (rest)->integer;
    }

  return INTEGER (sum, lm);
}

Cell *
eval_sub (Cell *args, LM *lm)
{
  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "sub: argument is not a list", lm);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "sub: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "sub: argument is not integer", lm);

  Integer total = CAR (args)->integer;

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "sub: argument is not integer",
                      lm);

      total -= CAR (rest)->integer;
    }

  return INTEGER (total, lm);
}

Cell *
eval_mul (Cell *args, LM *lm)
{
  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "mul: argument is not a list", lm);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "mul: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "mul: argument is not integer", lm);

  Integer result = CAR (args)->integer;

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "mul: argument is not integer",
                      lm);

      result *= CAR (rest)->integer;
    }

  return INTEGER (result, lm);
}

Cell *
eval_div (Cell *args, LM *lm)
{
  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "div: argument is not a list", lm);

  if (IS_NIL (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "div: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "div: argument is not integer", lm);

  Integer result = CAR (args)->integer;

  for (Cell *rest = CDR (args); !IS_NIL (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "div: argument is not integer",
                      lm);

      if (CAR (rest)->integer == 0)
        return ERROR (ERR_DIVISION_BY_0, "div: argument is zero", lm);

      result /= CAR (rest)->integer;
    }

  return INTEGER (result, lm);
}
