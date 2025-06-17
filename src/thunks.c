#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "fmt.h"
#include "lm.h"
#include "prims.h"
#include "thunks.h"

typedef Cell *(*ThunkFn) (LM *lm, Cell *, Cell *);

typedef struct
{
  const char *name;
  bool        is_lispm;
  int         arity;
  ThunkFn     fn;
} Thunk;

// thunks

Cell *
thunk_append (LM *lm, Cell *fn, Cell *lst)
{
  if (!LISTP (lst))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  Cell *result = CAR (lst);

  for (Cell *cdr = CDR (lst); !NILP (cdr); cdr = CDR (cdr))
    result = append_list (lm, result, CAR (cdr));

  return result;
}

Cell *
thunk_butlast (LM *lm, Cell *fn, Cell *lst)
{
  if (!LISTP (lst))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return butlast (lm, CAR (lst));
}

Cell *
thunk_cons (LM *lm, Cell *fn, Cell *args)
{
  (void)fn;
  return CONS (CAR (args), CAR (CDR (args)), lm);
}

Cell *
thunk_car (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;

  if (!LISTP (CAR (args)))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return CAR (CAR (args));
}

Cell *
thunk_cdr (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;

  Cell *car = CAR (args);

  if (!LISTP (car))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return CDR (car);
}

Cell *
thunk_last (LM *lm, Cell *fn, Cell *lst)
{
  if (!LISTP (lst))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return last (lm, CAR (lst));
}

Cell *
thunk_length (LM *lm, Cell *fn, Cell *lst)
{
  Cell *car = CAR (lst);

  if (!LISTP (lst))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return INTEGER (length (car), lm);
}

Cell *
thunk_nth (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;

  if (!IS_INST (args, CONS) || !IS_INST (CAR (args), INTEGER)
      || !LISTP (CAR (CDR (args))))
    return lm_err_nil (lm, ERR_ARG_NOT_ITERABLE, "%s i lst",
                       thunk_get_name (fn));

  size_t idx  = (size_t)CAR (args)->integer;
  Cell  *list = CAR (CDR (args));

  return nth (idx, list);
}

Cell *
thunk_print (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;

  if (!LISTP (args))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  PRINT (CAR (args));

  return T;
}

Cell *
thunk_reverse (LM *lm, Cell *fn, Cell *lst)
{
  if (!LISTP (lst))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return reverse (lm, CAR (lst));
}

Cell *
thunk_string (LM *lm, Cell *fn, Cell *args)
{
  (void)fn;
  return STRING (format (CAR (args)), lm);
}

// bool fns

Cell *
thunk_eq (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;
  (void)fn;

  Cell *car  = CAR (args);
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
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return nil_eq (NIL, CAR (args)) ? T : NIL;
}

// math fns

Cell *
thunk_gt (LM *lm, Cell *fn, Cell *args)
{
  (void)lm;
  (void)fn;

  Cell *result = T;

  if (!IS_INST (args, CONS))
    return lm_err_nil (lm, ERR_ARG_NOT_ITERABLE, "gt: is not a list");

  if (NILP (args))
    return lm_err_nil (lm, ERR_INVALID_ARG_LENGTH,
                       "gt: expected >= 1 arguments");

  if (!IS_INST (CAR (args), INTEGER))
    return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, "gt: is not an integer");

  Cell *prev = CAR (args);

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, "gt: is not an integer");

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
  (void)fn;

  Cell *result = T;

  if (!IS_INST (args, CONS))
    return lm_err_nil (lm, ERR_ARG_NOT_ITERABLE, "lt: is not a list");

  if (NILP (args))
    return lm_err_nil (lm, ERR_INVALID_ARG_LENGTH,
                       "lt: expected >= 1 arguments");

  if (!IS_INST (CAR (args), INTEGER))
    return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, "lt: is not an integer");

  Cell *prev = CAR (args);

  for (Cell *rest = CDR (args); !NILP (rest); rest = CDR (rest))
    {
      if (!IS_INST (CAR (rest), INTEGER))
        return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, "lt: is not an integer");

      if (!(prev->integer < CAR (rest)->integer))
        return NIL;

      prev = CAR (rest);
    }

  return result;
}

// TODO: check for over/underflow someday
Cell *
thunk_add (LM *lm, Cell *fn, Cell *args)
{
  (void)fn;
  const char *fmt = "add:%s";

  if (!LISTP (args))
    return lm_err_nil (lm, ERR_ARG_NOT_ITERABLE, fmt, "not a list");

  Cell *sum = INTEGER (0, lm);

  ConsIter iter = cons_iter (args);
  Cell    *item;

  while ((item = cons_next (&iter)))
    {
      if (!IS_INST (item, INTEGER))
        return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, fmt, "not an integer");

      sum->integer += item->integer;
    }

  return sum;
}

Cell *
thunk_sub (LM *lm, Cell *fn, Cell *args)
{
  (void)fn;
  const char *fmt = "sub:%s";

  if (!IS_INST (args, CONS) || NILP (args))
    return lm_err_nil (lm, ERR_ARG_NOT_ITERABLE, fmt, "not a list");

  Integer total = CAR (args)->integer;

  for (Cell *x = CDR (args); !NILP (x); x = CDR (x))
    {
      if (!IS_INST (CAR (x), INTEGER))
        return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, fmt, "not an integer");

      total -= CAR (x)->integer;
    }

  return INTEGER (total, lm);
}

Cell *
thunk_mul (LM *lm, Cell *fn, Cell *args)
{
  (void)fn;
  const char *fmt = "mul:%s";

  if (!LISTP (args))
    return lm_err_nil (lm, ERR_ARG_NOT_ITERABLE, fmt, "not a list");

  Integer res = 1;

  for (Cell *x = args; x != NIL; x = CDR (x))
    {
      if (!IS_INST (CAR (x), INTEGER))
        return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, fmt, "not an integer");

      res *= CAR (x)->integer;
    }

  return INTEGER (res, lm);
}

Cell *
thunk_div (LM *lm, Cell *fn, Cell *args)
{
  (void)fn;
  const char *fmt = "div:%s";

  if (!IS_INST (args, CONS) || NILP (args))
    return lm_err_nil (lm, ERR_ARG_NOT_ITERABLE, fmt, "not a list");

  Integer res = CAR (args)->integer;

  for (Cell *x = CDR (args); !NILP (x); x = CDR (x))
    {
      if (!IS_INST (CAR (x), INTEGER))
        return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, fmt, "not an integer");

      if (CAR (x)->integer == 0)
        return lm_err_nil (lm, ERR_DIVISION_BY_0, fmt, "is zero");

      res /= CAR (x)->integer;
    }

  return INTEGER (res, lm);
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
    return lm_err_nil (lm, ERR_INTERNAL, "null thunk");

  Thunk thunk = thunk_table[fn->thunk];

  if (!thunk.fn)
    return lm_err_nil (lm, ERR_NOT_A_FUNCTION,
                       thunk.name ?: "not a true thunk");

  int received = (int)length (arglist);

  if (thunk.arity > 0 && thunk.arity != received)
    {
      Err err
          = (received < thunk.arity) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
      return NIL; // FIXME
    }

  return thunk.fn (lm, fn, arglist);
}
