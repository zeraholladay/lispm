#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "eval.h"
#include "format.h"
#include "lispm_utils.h"
#include "xalloc.h"

// cond forms/expressions
static Cell *and_form (Cell *form, Context *ctx);
static Cell *if_form (Cell *form, Context *ctx);
static Cell *or_form (Cell *form, Context *ctx);

// (apply f arglist)
// (define (apply f . args)
//   (let* ((fixed-args   (butlast args))   ; all but the last element
//          (last-arg-list (last args))     ; the final element, as a list
//          (all-args     (append fixed-args last-arg-list)))
//     (funcall f all-args)))
Cell *
eval_apply (Cell *arglist, Context *ctx)
{
  Cell *fn = eval (CAR (arglist), ctx);

  Cell *fixed_rev = NIL;
  Cell *fixd_args = butlast (CDR (arglist), ctx);

  while (!IS_NIL (fixd_args))
    {
      Cell *eval_res = eval (CAR (fixd_args), ctx);
      fixed_rev = CONS (eval_res, fixed_rev, ctx);
      fixd_args = CDR (fixd_args);
    }

  Cell *last_arg_list = last (CDR (arglist), ctx);
  Cell *tail_list = eval (last_arg_list, ctx);

  if (!LISTP (tail_list))
    return ERROR (ERR_INVALID_ARG, fn, ctx);

  Cell *fixed = reverse_inplace (fixed_rev);
  Cell *all = append_inplace (fixed, tail_list);

  return funcall (fn, all, ctx);
}

Cell *
eval_funcall (Cell *args, Context *ctx)
{
  Cell *fn = eval (CAR (args), ctx);
  Cell *arglist = eval_list (CDR (args), ctx);
  return funcall (fn, arglist, ctx);
}

Cell *
eval_list (Cell *args, Context *ctx)
{
  Cell *rev = NIL;
  for (Cell *c = args; !IS_NIL (c); c = CDR (c))
    {
      Cell *eval_res = eval (CAR (c), ctx);
      rev = CONS (eval_res, rev, ctx);
    }
  return reverse_inplace (rev);
}

Cell *
eval (Cell *form, Context *ctx)
{
  if (IS (form, SYMBOL))
    return lookup (form, ctx);

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

      Cell *fn = eval (car, ctx);

      if (IS (fn, BUILTIN_FN) && fn->builtin_fn->is_lispm)
        {
          if (fn == KEYWORD (APPLY))
            return eval_apply (cdr, ctx);

          if (fn == KEYWORD (FUNCALL))
            return eval_funcall (cdr, ctx);

          if (fn == KEYWORD (EVAL))
            return eval (eval (CAR (cdr), ctx), ctx);

          if (fn == KEYWORD (PROGN))
            return eval_progn (cdr, ctx);

          if (fn == KEYWORD (AND))
            return and_form (cdr, ctx);

          if (fn == KEYWORD (IF))
            return if_form (cdr, ctx);

          if (fn == KEYWORD (OR))
            return or_form (cdr, ctx);

          return ERROR (ERR_INTERNAL, DEBUG_LOCATION, ctx);
        }

      Cell *arglist = eval_list (cdr, ctx);
      return funcall (fn, arglist, ctx);
    }

  return ERROR (ERR_INTERNAL, DEBUG_LOCATION, ctx);
}

Cell *
eval_progn (Cell *program, Context *ctx)
{
  Cell *result = NIL;

  for (Cell *forms = program; forms != NIL; forms = CDR (forms))
    {
      Cell *form = CAR (forms);
      result = eval (form, ctx);
    }

  return result;
}

// conditional forms/expressions
static Cell *
and_form (Cell *form, Context *ctx)
{
  Cell *eval_res = T;
  EqFn nil_eq = type (NIL)->eq;

  while (!IS_NIL (form))
    {
      eval_res = eval (CAR (form), ctx);
      if (nil_eq (NIL, eval_res))
        {
          return NIL;
        }
      form = CDR (form);
    }

  return eval_res;
}

static Cell *
if_form (Cell *form, Context *ctx)
{
  Cell *pred_form = CAR (form);

  if (!IS_NIL (eval (pred_form, ctx)))
    return eval (CAR (CDR (form)), ctx);
  else
    {
      Cell *else_form = CAR (CDR (CDR (form)));
      return else_form ? eval (else_form, ctx) : NIL;
    }
}

static Cell *
or_form (Cell *form, Context *ctx)
{
  Cell *eval_res = NIL;
  EqFn nil_eq = type (NIL)->eq;

  while (!IS_NIL (form))
    {
      eval_res = eval (CAR (form), ctx);

      if (!nil_eq (NIL, eval_res))
        return eval_res;

      form = CDR (form);
    }
  return NIL;
}

// other builtins

Cell *
eval_append (Cell *args, Context *ctx)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "append", ctx);

  Cell *result = CAR (args);

  for (Cell *lst = CDR (args); !IS_NIL (lst); lst = CDR (lst))
    result = append_list (result, CAR (lst), ctx);

  return result;
}

Cell *
eval_butlast (Cell *args, Context *ctx)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "butlast", ctx);

  return butlast (CAR (args), ctx);
}

Cell *
eval_cons (Cell *args, Context *ctx)
{
  return CONS (CAR (args), CAR (CDR (args)), ctx);
}

Cell *
eval_car (Cell *args, Context *ctx)
{
  (void)ctx;
  if (!LISTP (CAR (args)))
    return ERROR (ERR_INVALID_ARG, "car", ctx);

  return CAR (CAR (args));
}

Cell *
eval_cdr (Cell *args, Context *ctx)
{
  (void)ctx;
  Cell *car = CAR (args);

  if (!LISTP (car))
    return ERROR (ERR_INVALID_ARG, "cdr", ctx);

  return CDR (car);
}

Cell *
eval_length (Cell *args, Context *ctx)
{
  Cell *car = CAR (args);

  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "len", ctx);

  return INTEGER (length (car), ctx);
}

Cell *
eval_mapcar (Cell *args, Context *ctx)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "mapcar fn l1 ...", ctx);

  Cell *fn = CAR (args);

  if (IS_NOT (fn, BUILTIN_FN) && IS_NOT (fn, LAMBDA))
    return ERROR (ERR_INVALID_ARG, "mapcar: not a function or lambda", ctx);

  for (Cell *item = CDR (args); !IS_NIL (item); item = CDR (item))
    if (!LISTP (CAR (item)))
      return ERROR (ERR_INVALID_ARG, "mapcar: arg is not a list", ctx);

  return mapcar (fn, CDR (args), ctx);
}

Cell *
eval_nth (Cell *args, Context *ctx)
{
  (void)ctx;

  if (IS_NOT (args, CONS) || IS_NOT (CAR (args), INTEGER)
      || !LISTP (CAR (CDR (args))))
    return ERROR (ERR_ARG_NOT_ITERABLE, "nth: i list", ctx);

  size_t idx = (size_t)CAR (args)->integer;
  Cell *list = CAR (CDR (args));
  return nth (idx, list);
}

Cell *
eval_last (Cell *args, Context *ctx)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "last", ctx);

  return last (CAR (args), ctx);
}

Cell *
eval_print (Cell *args, Context *ctx)
{
  (void)ctx;
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "print", ctx);

  PRINT (CAR (args));
  return T;
}

Cell *
eval_reverse (Cell *args, Context *ctx)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "cdr", ctx);

  return reverse (CAR (args), ctx);
}

Cell *
eval_set (Cell *args, Context *ctx)
{
  if (IS_NOT (CAR (args), SYMBOL))
    return ERROR (ERR_INVALID_ARG, "set", ctx);

  return set (CAR (args), CAR (CDR (args)), ctx);
}

Cell *
eval_string (Cell *args, Context *ctx)
{
  return STRING (format (CAR (args)), ctx);
}
