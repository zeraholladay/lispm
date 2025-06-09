#include "stdlib.h"

#include "keywords.h"
#include "utils.h"
#include "xalloc.h"

// sequence operations

Cell *
append_inplace (Cell *lst1, Cell *lst2)
{
  if (NILP (lst1))
    return lst2;

  Cell *l1 = lst1;

  while (!NILP (CDR (l1)))
    l1 = CDR (l1);

  RPLACD (l1, lst2);

  return lst1;
}

Cell *
append_list (LM *lm, Cell *lst1, Cell *lst2)
{
  if (NILP (lst1))
    return lst2;

  Cell *new_head = NULL;
  Cell *new_tail = NULL;

  for (Cell *l1 = lst1; !NILP (l1); l1 = CDR (l1))
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

  RPLACD (new_tail, lst2);

  return new_head;
}

Cell *
butlast (LM *lm, Cell *lst)
{
  Cell *rev = reverse_inplace (lst);
  Cell *btl = reverse (lm, CDR (rev));
  reverse_inplace (rev);
  return btl;
}

Cell *
last (LM *lm, Cell *lst)
{
  (void)lm;
  Cell *rev = reverse_inplace (lst);
  Cell *last = CAR (rev);
  reverse_inplace (rev);
  return LIST1 (last, lm);
}

size_t
length (Cell *lst)
{
  if (!CONSP (lst))
    return 0;

  size_t i = 1;

  for (Cell *cdr = CDR (lst); cdr != NIL; cdr = CDR (cdr))
    ++i;

  return i;
}

Cell *
mapcar (LM *lm, Cell *fn, Cell *arglst)
{
  Cell *zip_args = zip (lm, arglst);
  Cell *rev = NIL;

  for (Cell *l = zip_args; !NILP (l); l = CDR (l))
    {
      Cell *res = lm_funcall (lm, fn, CAR (l));
      rev = CONS (res, rev, lm);
    }

  return reverse_inplace (rev);
}

Cell *
nth (size_t idx, Cell *lst)
{
  for (size_t i = 0; i < idx; ++i)
    {
      if (NILP (lst))
        return NIL;
      lst = CDR (lst);
    }

  return (NILP (lst)) ? NIL : CAR (lst);
}

Cell *
reverse (LM *lm, Cell *lst)
{
  (void)lm;
  Cell *result = NIL;

  for (Cell *l = lst; l != NIL; l = CDR (l))
    result = CONS (CAR (l), result, lm);

  return result;
}

Cell *
reverse_inplace (Cell *lst)
{
  Cell *prev = NIL;
  Cell *cur = lst;

  while (!NILP (cur))
    {
      Cell *next = CDR (cur);
      RPLACD (cur, prev);
      prev = cur;
      cur = next;
    }

  return prev;
}

Cell *
zip (LM *lm, Cell *lsts)
{
  scratch_t s;

  size_t len = length (lsts);
  if (len == 0)
    return NIL;

  Cell **heads = xalloc_scratch (&s, len * sizeof *heads);

  for (size_t i = 0; i < len; ++i)
    heads[i] = nth (i, lsts);

  Cell *out_rev = NIL;

  for (;;)
    {
      int done = 0;

      for (size_t i = 0; i < len; ++i)
        if (NILP (heads[i]))
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
lookup (LM *lm, Cell *cell)
{
  const char *key = cell->symbol.str;
  size_t len = cell->symbol.len;

  Cell *kywrd_cell = keyword_lookup (key, len);
  if (kywrd_cell)
    return kywrd_cell;

  Cell *res = lm_env_lkup (lm, key);
  if (!res)
    return ERROR (ERR_SYMBOL_NOT_FOUND, key, lm);

  return res;
}

Cell *
set (LM *lm, Cell *car, Cell *cdr)
{
  if (!IS_INST (car, SYMBOL))
    return ERROR (ERR_INVALID_ARG, "set", lm);

  const char *key = car->symbol.str;
  size_t len = car->symbol.len;

  if (keyword_lookup (key, len))
    return ERROR (ERR_INVALID_ARG, "set", lm);

  lm_env_set (lm, key, cdr);

  return cdr;
}
