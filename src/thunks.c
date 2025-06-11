#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "fmt.h"
#include "lm.h"
#include "prims.h"
#include "thunks.h"

typedef struct Cell *(*ThunkFn) (LM *lm, Cell *, Cell *);

typedef struct
{
  const char *name;
  bool is_lispm;
  int arity;
  ThunkFn fn;
} Thunk;

// thunks

Cell *
thunk_append (LM *lm, Cell *fn, Cell *lst)
{
  if (!LISTP (lst))
    LM_ERR_RET (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  Cell *result = CAR (lst);

  for (Cell *cdr = CDR (lst); !NILP (cdr); cdr = CDR (cdr))
    result = append_list (lm, result, CAR (cdr));

  return result;
}

Cell *
thunk_butlast (LM *lm, Cell *fn, Cell *lst)
{
  if (!LISTP (lst))
    LM_ERR_RET (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return butlast (lm, CAR (lst));
}

Cell *
thunk_cons (LM *lm, Cell *fn, Cell *args)
{
  return CONS (CAR (args), CAR (CDR (args)), lm);
}

Cell *
thunk_car (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;

  if (!LISTP (CAR (args)))
    LM_ERR_RET (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return CAR (CAR (args));
}

Cell *
thunk_cdr (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;

  Cell *car = CAR (args);

  if (!LISTP (car))
    LM_ERR_RET (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return CDR (car);
}

Cell *
thunk_last (LM *lm, Cell *fn, Cell *lst)
{
  if (!LISTP (lst))
    LM_ERR_RET (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return last (lm, CAR (lst));
}

Cell *
thunk_length (LM *lm, Cell *fn, Cell *lst)
{
  Cell *car = CAR (lst);

  if (!LISTP (lst))
    LM_ERR_RET (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return INTEGER (length (car), lm);
}

Cell *
thunk_mapcar (LM *lm, Cell *fn, Cell *args)
{
  if (!LISTP (args))
    LM_ERR_RET (lm, ERR_INVALID_ARG, "mapcar fn l1 ...");

  Cell *mapfn = CAR (args);

  if (!IS_INST (mapfn, THUNK) && !IS_INST (mapfn, LAMBDA))
    LM_ERR_RET (lm, ERR_INVALID_ARG, "mapcar: not a function or lambda");

  for (Cell *item = CDR (args); !NILP (item); item = CDR (item))
    if (!LISTP (CAR (item)))
      LM_ERR_RET (lm, ERR_INVALID_ARG, "mapcar: arg is not a list");

  return mapcar (lm, mapfn, CDR (args));
}

Cell *
thunk_nth (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;

  if (!IS_INST (args, CONS) || !IS_INST (CAR (args), INTEGER)
      || !LISTP (CAR (CDR (args))))
    LM_ERR_RET (lm, ERR_ARG_NOT_ITERABLE, "%s i lst", thunk_get_name (fn));

  size_t idx = (size_t)CAR (args)->integer;
  Cell *list = CAR (CDR (args));

  return nth (idx, list);
}

Cell *
thunk_print (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;

  if (!LISTP (args))
    LM_ERR_RET (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  PRINT (CAR (args));

  return T;
}

Cell *
thunk_reverse (LM *lm, Cell *fn, Cell *lst)
{
  if (!LISTP (lst))
    LM_ERR_RET (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return reverse (lm, CAR (lst));
}

Cell *
thunk_set (LM *lm, Cell *fn, Cell *args)
{
  if (!IS_INST (CAR (args), SYMBOL))
    LM_ERR_RET (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return set (lm, CAR (args), CAR (CDR (args)));
}

Cell *
thunk_string (LM *lm, Cell *fn, Cell *args)
{
  return STRING (format (CAR (args)), lm);
}

// bool fns

Cell *
thunk_eq (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;

  Cell *car = CAR (args);
  Cell *card = CAR (CDR (args));

  EqFn eqfn = type (car)->eq;

  if (eqfn (CAR (args), card))
    return T;

  return NIL;
}

Cell *
thunk_not (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;
  EqFn nil_eq = type (NIL)->eq;

  if (!LISTP (args))
    LM_ERR_RET (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return nil_eq (NIL, CAR (args)) ? T : NIL;
}

// math fns

Cell *
thunk_gt (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;
  Cell *result = T;

  if (!IS_INST (args, CONS))
    LM_ERR_RET (lm, ERR_ARG_NOT_ITERABLE, "gt: argument is not a list");

  if (NILP (args))
    LM_ERR_RET (lm, ERR_INVALID_ARG_LENGTH, "gt: expected >= 1 arguments");

  if (!IS_INST (CAR (args), INTEGER))
    LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH, "gt: argument is not an integer");

  Cell *prev = CAR (args);

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH,
                    "gt: argument is not an integer");

      if (!(prev->integer > CAR (rest)->integer))
        return NIL;

      prev = CAR (rest);
    }

  return result;
}

Cell *
thunk_lt (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;
  Cell *result = T;

  if (!IS_INST (args, CONS))
    LM_ERR_RET (lm, ERR_ARG_NOT_ITERABLE, "lt: argument is not a list");

  if (NILP (args))
    LM_ERR_RET (lm, ERR_INVALID_ARG_LENGTH, "lt: expected >= 1 arguments");

  if (!IS_INST (CAR (args), INTEGER))
    LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH, "lt: argument is not an integer");

  Cell *prev = CAR (args);

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH,
                    "lt: argument is not an integer");

      if (!(prev->integer < CAR (rest)->integer))
        return NIL;

      prev = CAR (rest);
    }

  return result;
}

// TODO: BUG IN gt (1 OR more args) and add, and mul take  (0 or more)
// TODO: check for over/underflow someday
Cell *
thunk_add (LM *lm, Cell *fn, Cell *args)
{
  if (!IS_INST (args, CONS))
    LM_ERR_RET (lm, ERR_ARG_NOT_ITERABLE, "add: argument is not a list");

  if (NILP (args))
    LM_ERR_RET (lm, ERR_INVALID_ARG_LENGTH, "add: expected >= 1 arguments");

  if (!IS_INST (CAR (args), INTEGER))
    LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH, "add: argument is not an integer");

  Integer sum = CAR (args)->integer;

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH,
                    "add: argument is not an integer");

      sum += CAR (rest)->integer;
    }

  return INTEGER (sum, lm);
}

Cell *
thunk_sub (LM *lm, Cell *fn, Cell *args)
{
  if (!IS_INST (args, CONS))
    LM_ERR_RET (lm, ERR_ARG_NOT_ITERABLE, "sub: argument is not a list");

  if (NILP (args))
    LM_ERR_RET (lm, ERR_INVALID_ARG_LENGTH, "sub: expected >= 1 arguments");

  if (!IS_INST (CAR (args), INTEGER))
    LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH, "sub: argument is not an integer");

  Integer total = CAR (args)->integer;

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH,
                    "sub: argument is not an integer");

      total -= CAR (rest)->integer;
    }

  return INTEGER (total, lm);
}

Cell *
thunk_mul (LM *lm, Cell *fn, Cell *args)
{
  if (!IS_INST (args, CONS))
    LM_ERR_RET (lm, ERR_ARG_NOT_ITERABLE, "mul: argument is not a list");

  if (NILP (args))
    LM_ERR_RET (lm, ERR_INVALID_ARG_LENGTH, "mul: expected >= 1 arguments");

  if (!IS_INST (CAR (args), INTEGER))
    LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH, "mul: argument is not an integer");

  Integer result = CAR (args)->integer;

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH,
                    "mul: argument is not an integer");

      result *= CAR (rest)->integer;
    }

  return INTEGER (result, lm);
}

Cell *
thunk_div (LM *lm, Cell *fn, Cell *args)
{
  if (!IS_INST (args, CONS))
    LM_ERR_RET (lm, ERR_ARG_NOT_ITERABLE, "div: argument is not a list");

  if (NILP (args))
    LM_ERR_RET (lm, ERR_INVALID_ARG_LENGTH, "div: expected >= 1 arguments");

  if (!IS_INST (CAR (args), INTEGER))
    LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH, "div: argument is not an integer");

  Integer result = CAR (args)->integer;

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        LM_ERR_RET (lm, ERR_ARG_TYPE_MISMATCH,
                    "div: argument is not an integer");

      if (CAR (rest)->integer == 0)
        LM_ERR_RET (lm, ERR_DIVISION_BY_0, "div: argument is zero");

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
  if (!fn)
    LM_ERR_RET (lm, ERR_INTERNAL, "null thunk");

  Thunk thunk = thunk_table[fn->thunk];

  if (!thunk.fn)
    LM_ERR_RET (lm, ERR_NOT_A_FUNCTION, thunk.name ?: "no name thunk");

  int received = (int)length (arglist);

  if (thunk.arity > 0 && thunk.arity != received)
    {
      ErrorCode err
          = (received < thunk.arity) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
      return NIL; // FIXME
    }

  return thunk.fn (lm, fn, arglist);
}
