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
thunk_append (LM *lm, Cell *fn, Cell *arglist)
{
  if (!LISTP (CAR (arglist)))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  Cell *result = CAR (arglist);

  for (Cell *cdr = CDR (arglist); !NILP (cdr); cdr = CDR (cdr))
    result = append_list (lm, result, CAR (cdr));

  return result;
}

Cell *
thunk_butlast (LM *lm, Cell *fn, Cell *arglist)
{
  if (!LISTP (CAR (arglist)))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return butlast (lm, CAR (arglist));
}

Cell *
thunk_cons (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;
  return CONS (CAR (arglist), CAR (CDR (arglist)), lm);
}

Cell *
thunk_car (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;

  if (!LISTP (CAR (arglist)))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return CAR (CAR (arglist));
}

Cell *
thunk_cdr (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;

  if (!LISTP (CAR (arglist)))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return CDR (CAR (arglist));
}

Cell *
thunk_last (LM *lm, Cell *fn, Cell *arglist)
{
  if (!LISTP (CAR (arglist)))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return last (lm, CAR (arglist));
}

Cell *
thunk_length (LM *lm, Cell *fn, Cell *arglist)
{
  if (!LISTP (CAR (arglist)))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return INTEGER (length (CAR (arglist)), lm);
}

Cell *
thunk_nth (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;

  if (!IS_INST (arglist, CONS) || !IS_INST (CAR (arglist), INTEGER)
      || !LISTP (CAR (CDR (arglist))))
    return lm_err_nil (lm, ERR_ARG_NOT_ITER, "%s i arglist",
                       thunk_get_name (fn));

  size_t idx  = (size_t)CAR (arglist)->integer;
  Cell  *list = CAR (CDR (arglist));

  return nth (idx, list);
}

Cell *
thunk_print (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;

  PRINT (CAR (arglist));

  return T;
}

Cell *
thunk_reverse (LM *lm, Cell *fn, Cell *arglist)
{
  if (!LISTP (CAR (arglist)))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return reverse (lm, CAR (arglist));
}

Cell *
thunk_string (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;
  return STRING (format (CAR (arglist)), lm);
}

// bool fns

Cell *
thunk_eq (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;
  (void)fn;

  Cell *car  = CAR (arglist);
  Cell *card = CAR (CDR (arglist));

  EqFn eqfn = type (car)->eq;

  if (eqfn (CAR (arglist), card))
    return T;

  return NIL;
}

Cell *
thunk_not (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;
  (void)fn;
  return CAR (arglist) == NIL ? T : NIL;
}

// math fns

Cell *
thunk_gt (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;
  (void)fn;

  Cell *result = T;

  if (!IS_INST (arglist, CONS))
    return lm_err_nil (lm, ERR_ARG_NOT_ITER, "gt: is not a list");

  if (NILP (arglist))
    return lm_err_nil (lm, ERR_INVALID_ARG_LEN, "gt: expected >= 1 arguments");

  if (!IS_INST (CAR (arglist), INTEGER))
    return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, "gt: is not an integer");

  Cell *prev = CAR (arglist);

  for (Cell *rest = CDR (arglist); !NILP (rest); rest = CDR (rest))
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
thunk_lt (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;
  (void)fn;

  Cell *result = T;

  if (!IS_INST (arglist, CONS))
    return lm_err_nil (lm, ERR_ARG_NOT_ITER, "lt: is not a list");

  if (NILP (arglist))
    return lm_err_nil (lm, ERR_INVALID_ARG_LEN, "lt: expected >= 1 arguments");

  if (!IS_INST (CAR (arglist), INTEGER))
    return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, "lt: is not an integer");

  Cell *prev = CAR (arglist);

  for (Cell *rest = CDR (arglist); !NILP (rest); rest = CDR (rest))
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
thunk_add (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;

  Cell *total = INTEGER (0, lm);
  Cell *item;

  for (ConsIter iter = cons_iter (arglist); (item = cons_next (&iter));)
    {
      if (!IS_INST (item, INTEGER))
        return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, "add: not an integer");
      total->integer += item->integer;
    }

  return total;
}

Cell *
thunk_sub (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;

  if (arglist == NIL)
    return lm_err_nil (lm, ERR_ARG_NOT_ITER, "sub: not a list");

  ConsIter iter = cons_iter (arglist);
  Cell    *item = cons_next (&iter);

  if (!IS_INST (item, INTEGER))
    return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, "sub: not an integer");

  Cell *res = INTEGER (item->integer, lm);
  Cell *cdr = cons_next (&iter);

  if (!cdr)
    {
      res->integer *= -1;
      return res;
    }
  do
    {
      if (!IS_INST (cdr, INTEGER))
        return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, "sub: not an integer");

      res->integer -= cdr->integer;
    }
  while ((cdr = cons_next (&iter)));

  return res;
}

Cell *
thunk_mul (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;
  const char *fmt = "mul:%s";

  if (!LISTP (arglist))
    return lm_err_nil (lm, ERR_ARG_NOT_ITER, fmt, "not a list");

  Integer res = 1;

  for (Cell *x = arglist; x != NIL; x = CDR (x))
    {
      if (!IS_INST (CAR (x), INTEGER))
        return lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, fmt, "not an integer");

      res *= CAR (x)->integer;
    }

  return INTEGER (res, lm);
}

Cell *
thunk_div (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;
  const char *fmt = "div:%s";

  if (!IS_INST (arglist, CONS) || NILP (arglist))
    return lm_err_nil (lm, ERR_ARG_NOT_ITER, fmt, "not a list");

  Integer res = CAR (arglist)->integer;

  for (Cell *x = CDR (arglist); !NILP (x); x = CDR (x))
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
thunk_sf_bool (Cell *c)
{
  return thunk_table[c->thunk].is_lispm;
}

Cell *
thunker (LM *lm, Cell *fn, Cell *arglist)
{
  if (!fn)
    return lm_err_nil (lm, ERR_INTERNAL, "null thunk");

  Thunk thunk = thunk_table[fn->thunk];

  if (!thunk.fn)
    return lm_err_nil (lm, ERR_INTERNAL, thunk.name ?: "not a thunk");

  int received = (int)length (arglist);

  if (thunk.arity > 0 && thunk.arity != received)
    {
      Err err
          = (received < thunk.arity) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
      return lm_err_nil (lm, err, "%s", thunk.name);
    }

  return thunk.fn (lm, fn, arglist);
}
