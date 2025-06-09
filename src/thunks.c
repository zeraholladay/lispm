#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "err.h"
#include "fmt.h"
#include "keywords.h"
#include "thunks.h"
#include "utils.h"

typedef struct Cell *(*ThunkFn) (LM *lm, struct Cell *);

typedef struct
{
  const char *name;
  bool is_lispm;
  int arity;
  ThunkFn fn;
} Thunk;

// thunks

Cell *
thunk_append (LM *lm, Cell *lst)
{
  if (!LISTP (lst))
    return ERROR (ERR_INVALID_ARG, "append", lm);

  Cell *result = CAR (lst);

  for (Cell *cdr = CDR (lst); !NILP (cdr); cdr = CDR (cdr))
    result = append_list (lm, result, CAR (cdr));

  return result;
}

Cell *
thunk_butlast (LM *lm, Cell *lst)
{
  if (!LISTP (lst))
    return ERROR (ERR_INVALID_ARG, "butlast", lm);

  return butlast (lm, CAR (lst));
}

Cell *
thunk_cons (LM *lm, Cell *args)
{
  return CONS (CAR (args), CAR (CDR (args)), lm);
}

Cell *
thunk_car (LM *lm, Cell *args)
{
  (void)lm;

  if (!LISTP (CAR (args)))
    return ERROR (ERR_INVALID_ARG, "car", lm);

  return CAR (CAR (args));
}

Cell *
thunk_cdr (LM *lm, Cell *args)
{
  (void)lm;

  Cell *car = CAR (args);

  if (!LISTP (car))
    return ERROR (ERR_INVALID_ARG, "cdr", lm);

  return CDR (car);
}

Cell *
thunk_last (LM *lm, Cell *lst)
{
  if (!LISTP (lst))
    return ERROR (ERR_INVALID_ARG, "last", lm);

  return last (lm, CAR (lst));
}

Cell *
thunk_length (LM *lm, Cell *lst)
{
  Cell *car = CAR (lst);

  if (!LISTP (lst))
    return ERROR (ERR_INVALID_ARG, "len", lm);

  return INTEGER (length (car), lm);
}

Cell *
thunk_mapcar (LM *lm, Cell *args)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "mapcar fn l1 ...", lm);

  Cell *fn = CAR (args);

  if (!IS_INST (fn, THUNK) && !IS_INST (fn, LAMBDA))
    return ERROR (ERR_INVALID_ARG, "mapcar: not a function or lambda", lm);

  for (Cell *item = CDR (args); !NILP (item); item = CDR (item))
    if (!LISTP (CAR (item)))
      return ERROR (ERR_INVALID_ARG, "mapcar: arg is not a list", lm);

  return mapcar (lm, fn, CDR (args));
}

Cell *
thunk_nth (LM *lm, Cell *args)
{
  (void)lm;

  if (!IS_INST (args, CONS) || !IS_INST (CAR (args), INTEGER)
      || !LISTP (CAR (CDR (args))))
    return ERROR (ERR_ARG_NOT_ITERABLE, "nth: i list", lm);

  size_t idx = (size_t)CAR (args)->integer;
  Cell *list = CAR (CDR (args));

  return nth (idx, list);
}

Cell *
thunk_print (LM *lm, Cell *args)
{
  (void)lm;

  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "print", lm);

  PRINT (CAR (args));

  return T;
}

Cell *
thunk_reverse (LM *lm, Cell *lst)
{
  if (!LISTP (lst))
    return ERROR (ERR_INVALID_ARG, "cdr", lm);

  return reverse (lm, CAR (lst));
}

Cell *
thunk_set (LM *lm, Cell *args)
{
  if (!IS_INST (CAR (args), SYMBOL))
    return ERROR (ERR_INVALID_ARG, "set", lm);

  return set (lm, CAR (args), CAR (CDR (args)));
}

Cell *
thunk_string (LM *lm, Cell *args)
{
  return STRING (format (CAR (args)), lm);
}

// bool fns

Cell *
thunk_eq (LM *lm, Cell *args)
{
  (void)lm;

  Cell *car = CAR (args);
  Cell *card = CAR (CDR (args));

  EqFn fn = type (car)->eq;

  if (fn (CAR (args), card))
    return T;

  return NIL;
}

Cell *
thunk_not (LM *lm, Cell *args)
{
  (void)lm;
  EqFn nil_eq = type (NIL)->eq;

  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "not", lm);

  return nil_eq (NIL, CAR (args)) ? T : NIL;
}

// math fns

Cell *
thunk_gt (LM *lm, Cell *args)
{
  (void)lm;
  Cell *result = T;

  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "gt: argument is not a list", lm);

  if (NILP (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "gt: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "gt: argument is not an integer", lm);

  Cell *prev = CAR (args);

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "gt: argument is not an integer",
                      lm);

      if (!(prev->integer > CAR (rest)->integer))
        return NIL;

      prev = CAR (rest);
    }

  return result;
}

Cell *
thunk_lt (LM *lm, Cell *args)
{
  (void)lm;
  Cell *result = T;

  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "lt: argument is not a list", lm);

  if (NILP (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "lt: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "lt: argument is not an integer", lm);

  Cell *prev = CAR (args);

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "lt: argument is not an integer",
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
thunk_add (LM *lm, Cell *args)
{
  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "add: argument is not a list", lm);

  if (NILP (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "add: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "add: argument is not an integer",
                  lm);

  Integer sum = CAR (args)->integer;

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "add: argument is not an integer",
                      lm);

      sum += CAR (rest)->integer;
    }

  return INTEGER (sum, lm);
}

Cell *
thunk_sub (LM *lm, Cell *args)
{
  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "sub: argument is not a list", lm);

  if (NILP (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "sub: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "sub: argument is not an integer",
                  lm);

  Integer total = CAR (args)->integer;

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "sub: argument is not an integer",
                      lm);

      total -= CAR (rest)->integer;
    }

  return INTEGER (total, lm);
}

Cell *
thunk_mul (LM *lm, Cell *args)
{
  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "mul: argument is not a list", lm);

  if (NILP (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "mul: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "mul: argument is not an integer",
                  lm);

  Integer result = CAR (args)->integer;

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "mul: argument is not an integer",
                      lm);

      result *= CAR (rest)->integer;
    }

  return INTEGER (result, lm);
}

Cell *
thunk_div (LM *lm, Cell *args)
{
  if (!IS_INST (args, CONS))
    return ERROR (ERR_ARG_NOT_ITERABLE, "div: argument is not a list", lm);

  if (NILP (args))
    return ERROR (ERR_INVALID_ARG_LENGTH, "div: expected >= 1 arguments", lm);

  if (!IS_INST (CAR (args), INTEGER))
    return ERROR (ERR_ARG_TYPE_MISMATCH, "div: argument is not an integer",
                  lm);

  Integer result = CAR (args)->integer;

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return ERROR (ERR_ARG_TYPE_MISMATCH, "div: argument is not an integer",
                      lm);

      if (CAR (rest)->integer == 0)
        return ERROR (ERR_DIVISION_BY_0, "div: argument is zero", lm);

      result /= CAR (rest)->integer;
    }

  return INTEGER (result, lm);
}

static const Thunk thunk_table[_THUNK_END] = {
#define X(sym, is_l, ar, fn) { #sym, is_l, ar, fn },
#include "thunks.def"
#undef X
};

const char *
thunk_get_name (Cell *c)
{
  return (IS_INST (c, THUNK)) ? thunk_table[c->thunk].name : "bad thunk!";
}

bool
thunk_is_lispm (Cell *c)
{
  return thunk_table[c->thunk].is_lispm ? true : false;
}

Cell *
thunker (LM *lm, Cell *fn, Cell *arglist)
{
  Thunk thunk = thunk_table[fn->thunk];

  int received = (int)length (arglist);

  if (thunk.arity > 0 && thunk.arity != received)
    {
      ErrorCode err
          = (received < thunk.arity) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
      return NIL; // FIXME
    }

  // if (!thunk.fn)
  //   ERR_EXIT (ERR_NOT_A_FUNCTION, thunk.name);

  return thunk.fn (lm, arglist);
}
