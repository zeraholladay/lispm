#include "stdlib.h"

#include "eval.h"
#include "lispm_utils.h"
#include "xalloc.h"

// funcall & eval
Cell *
funcall (Cell *fn, Cell *arglist, LM *lm)
{
  if (IS (fn, BUILTIN_FN))
    return funcall_builtin (fn, arglist, lm);

  if (IS (fn, LAMBDA))
    return funcall_lambda (fn, arglist, lm);

  return ERROR (ERR_NOT_A_FUNCTION, fn, lm);
}

Cell *
funcall_builtin (Cell *fn, Cell *arglist, LM *lm)
{
  int received = (int)length (arglist);
  const BuiltinFn *builtin_fn = fn->builtin_fn;

  if (builtin_fn->arity > 0 && builtin_fn->arity != received)
    {
      ErrorCode err = (received < builtin_fn->arity) ? ERR_MISSING_ARG
                                                     : ERR_UNEXPECTED_ARG;
      return ERROR (err, builtin_fn->name, lm);
    }

  // eval_apply or eval_funcall could have taken us here.
  // so if we called them again, arglist would be eval'd 2x.
  if (fn == KEYWORD (FUNCALL))
    {
      Cell *fn2 = eval (CAR (arglist), lm);
      return funcall (fn2, CDR (arglist), lm);
    }

  if (fn == KEYWORD (APPLY))
    {
      Cell *fn2 = eval (CAR (arglist), lm);
      return funcall (fn2, CAR (CDR (arglist)), lm);
    }

  if (fn == KEYWORD (LIST))
    return arglist; // LIST is eval_list, so we're done.

  return builtin_fn->fn (arglist, lm);
}

Cell *
funcall_lambda (Cell *fn, Cell *args, LM *lm)
{
  size_t expected = length (fn->lambda.params);
  size_t received = length (args);

  if (expected != received)
    {
      ErrorCode err
          = (received < expected) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
      return ERROR (err, fn, lm);
    }

  env_enter_frame (&lm->env);

  Cell *pairs
      = mapcar (KEYWORD (LIST), LIST2 (fn->lambda.params, args, lm), lm);

  while (!IS_NIL (pairs))
    {
      Cell *pair = CAR (pairs);
      env_let (lm->env, (CAR (pair))->symbol.str, CADR (pair));
      pairs = CDR (pairs);
    }

  Cell *res = eval_progn (fn->lambda.body, lm);

  env_leave_frame (&lm->env);

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
append_list (Cell *list1, Cell *list2, LM *lm)
{
  if (IS_NIL (list1))
    return list2;

  Cell *new_head = NULL;
  Cell *new_tail = NULL;

  for (Cell *l1 = list1; !IS_NIL (l1); l1 = CDR (l1))
    {
      Cell *cpy = CONS (CAR (l1), NIL, lm);

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
butlast (Cell *list, LM *lm)
{
  Cell *rev = reverse_inplace (list);
  Cell *btl = reverse (CDR (rev), lm);
  reverse_inplace (rev);
  return btl;
}

Cell *
last (Cell *list, LM *lm)
{
  (void)lm;
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
mapcar (Cell *fn, Cell *arglist, LM *lm)
{
  Cell *zip_args = zip (arglist, lm);
  Cell *rev = NIL;

  for (Cell *l = zip_args; !IS_NIL (l); l = CDR (l))
    {
      Cell *res = funcall (fn, CAR (l), lm);
      rev = CONS (res, rev, lm);
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
reverse (Cell *list, LM *lm)
{
  (void)lm;
  Cell *result = NIL;

  for (Cell *l = list; l != NIL; l = CDR (l))
    result = CONS (CAR (l), result, lm);

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
zip (Cell *lists, LM *lm)
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
          row_rev = CONS (CAR (heads[i]), row_rev, lm);
          heads[i] = CDR (heads[i]);
        }

      out_rev = CONS (reverse_inplace (row_rev), out_rev, lm);
    }

  xfree_scratch (&s);
  return reverse_inplace (out_rev);
}

// context operations
Cell *
lookup (Cell *cell, LM *lm)
{
  const char *key = cell->symbol.str;
  size_t len = cell->symbol.len;

  Cell *kywrd_cell = keyword_lookup (key, len);
  if (kywrd_cell)
    return kywrd_cell;

  Cell *res = env_lookup (lm->env, key);
  if (!res)
    return ERROR (ERR_SYMBOL_NOT_FOUND, key, lm);

  return res;
}

Cell *
set (Cell *car, Cell *cdr, LM *lm)
{
  if (!IS (car, SYMBOL))
    return ERROR (ERR_INVALID_ARG, "set", lm);

  const char *key = car->symbol.str;
  size_t len = car->symbol.len;

  if (keyword_lookup (key, len))
    return ERROR (ERR_INVALID_ARG, "set", lm);

  env_set (lm->env, key, cdr);
  return cdr;
}
