#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "eval.h"
#include "format.h"
#include "lispm_utils.h"
#include "xalloc.h"

// builtins

Cell *
eval_append (Cell *args, LM *lm)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "append", lm);

  Cell *result = CAR (args);

  for (Cell *lst = CDR (args); !IS_NIL (lst); lst = CDR (lst))
    result = append_list (result, CAR (lst), lm);

  return result;
}

Cell *
eval_butlast (Cell *args, LM *lm)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "butlast", lm);

  return butlast (CAR (args), lm);
}

Cell *
eval_cons (Cell *args, LM *lm)
{
  return CONS (CAR (args), CAR (CDR (args)), lm);
}

Cell *
eval_car (Cell *args, LM *lm)
{
  (void)lm;
  if (!LISTP (CAR (args)))
    return ERROR (ERR_INVALID_ARG, "car", lm);

  return CAR (CAR (args));
}

Cell *
eval_cdr (Cell *args, LM *lm)
{
  (void)lm;
  Cell *car = CAR (args);

  if (!LISTP (car))
    return ERROR (ERR_INVALID_ARG, "cdr", lm);

  return CDR (car);
}

Cell *
eval_length (Cell *args, LM *lm)
{
  Cell *car = CAR (args);

  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "len", lm);

  return INTEGER (length (car), lm);
}

Cell *
eval_mapcar (Cell *args, LM *lm)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "mapcar fn l1 ...", lm);

  Cell *fn = CAR (args);

  if (!IS_INST (fn, BUILTIN_FN) && !IS_INST (fn, LAMBDA))
    return ERROR (ERR_INVALID_ARG, "mapcar: not a function or lambda", lm);

  for (Cell *item = CDR (args); !IS_NIL (item); item = CDR (item))
    if (!LISTP (CAR (item)))
      return ERROR (ERR_INVALID_ARG, "mapcar: arg is not a list", lm);

  return mapcar (fn, CDR (args), lm);
}

Cell *
eval_nth (Cell *args, LM *lm)
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
eval_last (Cell *args, LM *lm)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "last", lm);

  return last (CAR (args), lm);
}

Cell *
eval_print (Cell *args, LM *lm)
{
  (void)lm;
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "print", lm);

  PRINT (CAR (args));
  return T;
}

Cell *
eval_reverse (Cell *args, LM *lm)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "cdr", lm);

  return reverse (CAR (args), lm);
}

Cell *
eval_set (Cell *args, LM *lm)
{
  if (!IS_INST (CAR (args), SYMBOL))
    return ERROR (ERR_INVALID_ARG, "set", lm);

  return set (CAR (args), CAR (CDR (args)), lm);
}

Cell *
eval_string (Cell *args, LM *lm)
{
  return STRING (format (CAR (args)), lm);
}
