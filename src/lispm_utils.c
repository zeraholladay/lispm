#include "stdlib.h"

#include "eval.h"
#include "lispm_utils.h"
#include "xalloc.h"

// funcall & eval
Cell *
funcall (Cell *fn, Cell *arglist, Context *ctx)
{
  if (IS (fn, BUILTIN_FN))
    return funcall_builtin (fn, arglist, ctx);

  if (IS (fn, LAMBDA))
    return funcall_lambda (fn, arglist, ctx);

  return ERROR (ERR_NOT_A_FUNCTION, fn, ctx);
}

Cell *
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

Cell *
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

// sequence operations

Cell *
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

// APPEND function concatenates list arguments into one list.
// Resulting list is shallow cpy of specified lists except for the last which
// is directly shared.
Cell *
append_list (Cell *list1, Cell *list2, Context *ctx)
{
  if (IS_NIL (list1))
    return list2;

  Cell *new_head = NULL;
  Cell *new_tail = NULL;

  for (Cell *l1 = list1; !IS_NIL (l1); l1 = CDR (l1))
    {
      Cell *cpy = CONS (CAR (l1), NIL, ctx);

      if (new_head == NULL)
        {
          new_head = cpy;
          new_tail = cpy;
        }
      else
        {
          RPLACD (new_tail, cpy);
          new_tail = cpy;
        }
    }

  RPLACD (new_tail, list2);

  return new_head;
}

Cell *
butlast (Cell *list, Context *ctx)
{
  Cell *rev = reverse_inplace (list);
  Cell *btl = reverse (CDR (rev), ctx);
  reverse_inplace (rev);
  return btl;
}

Cell *
last (Cell *list, Context *ctx)
{
  (void)ctx;
  Cell *rev = reverse_inplace (list);
  Cell *last = CAR (rev);
  reverse_inplace (rev);
  return last;
}

size_t
length (Cell *list)
{
  if (!IS (list, CONS))
    return 0;

  size_t i = 1;

  for (Cell *cdr = CDR (list); cdr != NIL; cdr = CDR (cdr))
    ++i;

  return i;
}

Cell *
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

Cell *
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

Cell *
reverse (Cell *list, Context *ctx)
{
  (void)ctx;
  Cell *result = NIL;

  for (Cell *l = list; l != NIL; l = CDR (l))
    result = CONS (CAR (l), result, ctx);

  return result;
}

Cell *
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

Cell *
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
Cell *
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

Cell *
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
