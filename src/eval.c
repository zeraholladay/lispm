#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "eval.h"
#include "format.h"
#include "lispm_utils.h"
#include "xalloc.h"

// cond forms/expressions
static Cell *and_form (Cell *form, LM *lm);
static Cell *if_form (Cell *form, LM *lm);
static Cell *or_form (Cell *form, LM *lm);

// (apply f arglist)
// (define (apply f . args)
//   (let* ((fixed-args   (butlast args))   ; all but the last element
//          (last-arg-list (last args))     ; the final element, as a list
//          (all-args     (append fixed-args last-arg-list)))
//     (funcall f all-args)))
Cell *
eval_apply (Cell *arglist, LM *lm)
{
  Cell *fn = eval (CAR (arglist), lm);

  Cell *fixed_rev = NIL;
  Cell *fixd_args = butlast (CDR (arglist), lm);

  while (!IS_NIL (fixd_args))
    {
      Cell *eval_res = eval (CAR (fixd_args), lm);
      fixed_rev = CONS (eval_res, fixed_rev, lm);
      fixd_args = CDR (fixd_args);
    }

  Cell *last_arg_list = last (CDR (arglist), lm);
  Cell *tail_list = eval (last_arg_list, lm);

  if (!LISTP (tail_list))
    return ERROR (ERR_INVALID_ARG, fn, lm);

  Cell *fixed = reverse_inplace (fixed_rev);
  Cell *all = append_inplace (fixed, tail_list);

  return funcall (fn, all, lm);
}

Cell *
eval_funcall (Cell *args, LM *lm)
{
  Cell *fn = eval (CAR (args), lm);
  Cell *arglist = eval_list (CDR (args), lm);
  return funcall (fn, arglist, lm);
}

Cell *
eval_list (Cell *args, LM *lm)
{
  Cell *rev = NIL;
  for (Cell *c = args; !IS_NIL (c); c = CDR (c))
    {
      Cell *eval_res = eval (CAR (c), lm);
      rev = CONS (eval_res, rev, lm);
    }
  return reverse_inplace (rev);
}

Cell *
eval (Cell *form, LM *lm)
{
  if (IS (form, SYMBOL))
    return lookup (form, lm);

  // literals, numbers, strings, etc.
  if (!LISTP (form))
    return form;

  if (LISTP (form))
    {
      if (IS_NIL (form))
        return NIL;

      Cell *car = CAR (form);
      Cell *cdr = CDR (form);

      if (car == KEYWORD (QUOTE))
        return CAR (cdr);

      if (IS (car, LAMBDA))
        return car;

      Cell *fn = eval (car, lm);

      if (IS (fn, BUILTIN_FN) && fn->builtin_fn->is_lispm)
        {
          if (fn == KEYWORD (APPLY))
            return eval_apply (cdr, lm);

          if (fn == KEYWORD (FUNCALL))
            return eval_funcall (cdr, lm);

          if (fn == KEYWORD (EVAL))
            return eval (eval (CAR (cdr), lm), lm);

          if (fn == KEYWORD (PROGN))
            return eval_progn (cdr, lm);

          if (fn == KEYWORD (AND))
            return and_form (cdr, lm);

          if (fn == KEYWORD (IF))
            return if_form (cdr, lm);

          if (fn == KEYWORD (OR))
            return or_form (cdr, lm);

          return ERROR (ERR_INTERNAL, DEBUG_LOCATION, lm);
        }

      Cell *arglist = eval_list (cdr, lm);
      return funcall (fn, arglist, lm);
    }

  return ERROR (ERR_INTERNAL, DEBUG_LOCATION, lm);
}

Cell *
eval_progn (Cell *program, LM *lm)
{
  Cell *result = NIL;

  for (Cell *forms = program; forms != NIL; forms = CDR (forms))
    {
      Cell *form = CAR (forms);
      result = eval (form, lm);
    }

  return result;
}

// conditional forms/expressions
static Cell *
and_form (Cell *form, LM *lm)
{
  Cell *eval_res = T;
  EqFn nil_eq = type (NIL)->eq;

  while (!IS_NIL (form))
    {
      eval_res = eval (CAR (form), lm);
      if (nil_eq (NIL, eval_res))
        {
          return NIL;
        }
      form = CDR (form);
    }

  return eval_res;
}

static Cell *
if_form (Cell *form, LM *lm)
{
  Cell *pred_form = CAR (form);

  if (!IS_NIL (eval (pred_form, lm)))
    return eval (CAR (CDR (form)), lm);
  else
    {
      Cell *else_form = CAR (CDR (CDR (form)));
      return else_form ? eval (else_form, lm) : NIL;
    }
}

static Cell *
or_form (Cell *form, LM *lm)
{
  Cell *eval_res = NIL;
  EqFn nil_eq = type (NIL)->eq;

  while (!IS_NIL (form))
    {
      eval_res = eval (CAR (form), lm);
      if (!nil_eq (NIL, eval_res))
        return eval_res;

      form = CDR (form);
    }
  return NIL;
}

// other builtins

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

  if (IS_NOT (fn, BUILTIN_FN) && IS_NOT (fn, LAMBDA))
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

  if (IS_NOT (args, CONS) || IS_NOT (CAR (args), INTEGER)
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
  if (IS_NOT (CAR (args), SYMBOL))
    return ERROR (ERR_INVALID_ARG, "set", lm);

  return set (CAR (args), CAR (CDR (args)), lm);
}

Cell *
eval_string (Cell *args, LM *lm)
{
  return STRING (format (CAR (args)), lm);
}
