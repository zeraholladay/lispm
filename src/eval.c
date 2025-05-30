#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "eval.h"
#include "format.h"
#include "xalloc.h"

// funcalls
static Cell *funcall (Cell *fn, Cell *arglist, Context *ctx);
static Cell *funcall_builtin (Cell *fn, Cell *args, Context *ctx);
static Cell *funcall_lambda (Cell *fn, Cell *args, Context *ctx);

// cond forms/expressions
static Cell *and_form (Cell *form, Context *ctx);
static Cell *if_form (Cell *form, Context *ctx);
static Cell *or_form (Cell *form, Context *ctx);

// sequence operations
static Cell *append_inplace (Cell *list1, Cell *list2);
static Cell *append_list (Cell *list1, Cell *list2, Context *ctx);
static Cell *butlast (Cell *args, Context *ctx);
static Cell *last (Cell *args, Context *ctx);
static size_t length (Cell *list);
static Cell *mapcar (Cell *fn, Cell *arglist, Context *ctx);
static Cell *nth (size_t idx, Cell *list);
static Cell *reverse (Cell *list, Context *ctx);
static Cell *reverse_inplace (Cell *list);
static Cell *zip (Cell *lists, Context *ctx);

// context operations
static Cell *lookup (Cell *cell, Context *ctx);
static Cell *set (Cell *car, Cell *cdr, Context *ctx);

// funcall & eval
static Cell *
funcall (Cell *fn, Cell *arglist, Context *ctx)
{
  if (IS (fn, BUILTIN_FN))
    return funcall_builtin (fn, arglist, ctx);

  if (IS (fn, LAMBDA))
    return funcall_lambda (fn, arglist, ctx);

  return ERROR (ERR_NOT_A_FUNCTION, fn, ctx);
}

static Cell *
funcall_builtin (Cell *fn, Cell *arglist, Context *ctx)
{
  int received = (int)length (arglist);
  const BuiltinFn *builtin_fn = fn->builtin_fn;

  if (builtin_fn->arity > 0 && builtin_fn->arity != received)
    {
      ErrorCode err = (received < builtin_fn->arity) ? ERR_MISSING_ARG
                                                     : ERR_UNEXPECTED_ARG;
      return ERROR (err, builtin_fn->name, ctx);
    }

  // eval_apply or eval_funcall could have taken us here.
  // so if we called them again, arglist would be eval'd 2x.
  if (fn == KEYWORD (FUNCALL))
    {
      Cell *fn2 = eval (CAR (arglist), ctx);
      return funcall (fn2, CDR (arglist), ctx);
    }

  if (fn == KEYWORD (APPLY))
    {
      Cell *fn2 = eval (CAR (arglist), ctx);
      return funcall (fn2, CAR (CDR (arglist)), ctx);
    }

  if (fn == KEYWORD (LIST))
    return arglist; // LIST is eval_list, so we're done.

  return builtin_fn->fn (arglist, ctx);
}

static Cell *
funcall_lambda (Cell *fn, Cell *args, Context *ctx)
{
  size_t expected = length (fn->lambda.params);
  size_t received = length (args);

  if (expected != received)
    {
      ErrorCode err
          = (received < expected) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
      return ERROR (err, fn, ctx);
    }

  env_enter_frame (&ctx->env);

  Cell *pairs
      = mapcar (KEYWORD (LIST), LIST2 (fn->lambda.params, args, ctx), ctx);

  while (!IS_NIL (pairs))
    {
      Cell *pair = CAR (pairs);
      env_let (ctx->env, (CAR (pair))->symbol.str, CADR (pair));
      pairs = CDR (pairs);
    }

  Cell *res = eval_progn (fn->lambda.body, ctx);

  env_leave_frame (&ctx->env);

  return res;
}

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
  if (IS_NIL (args))
    return NIL;

  Cell *car = eval (CAR (args), ctx);
  Cell *cdr = eval_list (CDR (args), ctx);

  return CONS (car, cdr, ctx);
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

      if (IS (fn, BUILTIN_FN) && fn->builtin_fn->sform)
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
        {
          return eval_res;
        }

      form = CDR (form);
    }
  return NIL;
}

// sequence operations

static Cell *
append_inplace (Cell *list1, Cell *list2)
{
  if (IS_NIL (list1))
    return list2;

  Cell *l1 = list1;

  while (!IS_NIL (CDR (l1)))
    l1 = CDR (l1);

  RPLACD (l1, list2);

  return list1;
}

static Cell *
append_list (Cell *list1, Cell *list2, Context *ctx)
{
  if (IS_NIL (list1))
    return list2;

  return CONS (CAR (list1), append_list (CDR (list1), list2, ctx), ctx);
}

static Cell *
butlast (Cell *list, Context *ctx)
{
  Cell *rev = reverse_inplace (list);
  Cell *btl = reverse (CDR (rev), ctx);
  reverse_inplace (rev);
  return btl;
}

static Cell *
last (Cell *list, Context *ctx)
{
  (void)ctx;
  Cell *rev = reverse_inplace (list);
  Cell *last = CAR (rev);
  reverse_inplace (rev);
  return last;
}

static size_t
length (Cell *list)
{
  if (!IS (list, CONS))
    return 0;

  size_t i = 1;

  for (Cell *cdr = CDR (list); cdr != NIL; cdr = CDR (cdr))
    ++i;

  return i;
}

static Cell *
mapcar (Cell *fn, Cell *arglist, Context *ctx)
{
  Cell *zip_args = zip (arglist, ctx);
  Cell *rev = NIL;

  for (Cell *l = zip_args; !IS_NIL (l); l = CDR (l))
    {
      Cell *res = funcall (fn, CAR (l), ctx);
      rev = CONS (res, rev, ctx);
    }

  return reverse_inplace (rev);
}

static Cell *
nth (size_t idx, Cell *list)
{
  for (size_t i = 0; i < idx; ++i)
    {
      if (IS_NIL (list))
        return NIL;
      list = CDR (list);
    }

  return (IS_NIL (list)) ? NIL : CAR (list);
}

static Cell *
reverse (Cell *list, Context *ctx)
{
  (void)ctx;
  Cell *result = NIL;

  for (Cell *l = list; l != NIL; l = CDR (l))
    result = CONS (CAR (l), result, ctx);

  return result;
}

static Cell *
reverse_inplace (Cell *list)
{
  Cell *prev = NIL;
  Cell *cur = list;

  while (!IS_NIL (cur))
    {
      Cell *next = CDR (cur);
      RPLACD (cur, prev);
      prev = cur;
      cur = next;
    }

  return prev;
}

static Cell *
zip (Cell *lists, Context *ctx)
{
  scratch_t s;

  size_t len = length (lists);
  if (len == 0)
    return NIL;

  Cell **heads = xalloc_scratch (&s, len * sizeof *heads);

  for (size_t i = 0; i < len; ++i)
    heads[i] = nth (i, lists);

  Cell *out_rev = NIL;

  for (;;)
    {
      int done = 0;

      for (size_t i = 0; i < len; ++i)
        if (IS_NIL (heads[i]))
          {
            done = 1;
            break;
          }

      if (done)
        break;

      Cell *row_rev = NIL;

      for (size_t i = 0; i < len; ++i)
        {
          row_rev = CONS (CAR (heads[i]), row_rev, ctx);
          heads[i] = CDR (heads[i]);
        }

      out_rev = CONS (reverse_inplace (row_rev), out_rev, ctx);
    }

  xfree_scratch (&s);
  return reverse_inplace (out_rev);
}

// context operations
static Cell *
lookup (Cell *cell, Context *ctx)
{
  const char *key = cell->symbol.str;
  size_t len = cell->symbol.len;

  Cell *kywrd_cell = keyword_lookup (key, len);
  if (kywrd_cell)
    return kywrd_cell;

  Cell *res = env_lookup (ctx->env, key);
  if (!res)
    return ERROR (ERR_SYMBOL_NOT_FOUND, key, ctx);

  return res;
}

static Cell *
set (Cell *car, Cell *cdr, Context *ctx)
{
  if (!IS (car, SYMBOL))
    return ERROR (ERR_INVALID_ARG, "set", ctx);

  const char *key = car->symbol.str;
  size_t len = car->symbol.len;

  if (keyword_lookup (key, len))
    return ERROR (ERR_INVALID_ARG, "set", ctx);

  env_set (ctx->env, key, cdr);
  return cdr;
}

// other builtins

Cell *
eval_append (Cell *args, Context *ctx)
{
  if (!LISTP (args))
    return ERROR (ERR_INVALID_ARG, "append", ctx);

  Cell *result = CAR (args);

  for (Cell *list = CDR (args); args != NIL; args = CDR (args))
    {
      result = append_list (result, CAR (list), ctx);
    }

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
