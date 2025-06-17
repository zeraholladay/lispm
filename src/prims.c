#include "stdlib.h"

#include "palloc.h"
#include "prims.h"
#include "xalloc.h"

#define AS_SYM(name) { .str = name, .len = sizeof (name) - 1 }

// NIL & T

PrimWrapper wrapped_t = {
  .free      = 0,
  .gc_mark   = 1,
  .next_free = NULL,
  .ptr       = { .type = TYPE_SYMBOL, .symbol = AS_SYM ("t") },
};

PrimWrapper wrapper_nil = {
  .free      = 0,
  .gc_mark   = 1,
  .next_free = NULL,
  .ptr       = { .type = TYPE_NIL, .cons = { .car = NULL, .cdr = NULL } },
};

PrimWrapper wrapper_quote = {
  .free      = 0,
  .gc_mark   = 1,
  .next_free = NULL,
  .ptr       = { .type = TYPE_SYMBOL, .symbol = AS_SYM ("quote") },
};

// sequence operations

Cell *
append_inplace (Cell *lst1, Cell *lst2)
{
  if (lst1 == NIL)
    return lst2;

  Cell *l1 = lst1;

  while (NIL != CDR (l1))
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

  for (Cell *l1 = lst1; l1 != NIL; l1 = CDR (l1))
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
  Cell *rev  = reverse_inplace (lst);
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
nth (size_t idx, Cell *lst)
{
  for (size_t i = 0; i < idx; ++i)
    {
      if (lst == NIL)
        return NIL;
      lst = CDR (lst);
    }

  return (NILP (lst)) ? NIL : CAR (lst);
}

Cell *
reverse (LM *lm, Cell *lst)
{
  Cell *result = NIL;

  for (Cell *l = lst; l != NIL; l = CDR (l))
    result = CONS (CAR (l), result, lm);

  return result;
}

Cell *
reverse_inplace (Cell *lst)
{
  Cell *prev = NIL;
  Cell *cur  = lst;

  while (cur != NIL)
    {
      Cell *next = CDR (cur);
      RPLACD (cur, prev);
      prev = cur;
      cur  = next;
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
        if (heads[i] == NIL)
          {
            done = 1;
            break;
          }

      if (done)
        break;

      Cell *row_rev = NIL;

      for (size_t i = 0; i < len; ++i)
        {
          row_rev  = CONS (CAR (heads[i]), row_rev, lm);
          heads[i] = CDR (heads[i]);
        }

      out_rev = CONS (reverse_inplace (row_rev), out_rev, lm);
    }

  xfree_scratch (&s);

  return reverse_inplace (out_rev);
}
