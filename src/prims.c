#include "prims.h"

#define AS_SYM(name) { .str = name, .len = sizeof (name) - 1 }

PrimWrapper wrapped_t = {
  .free      = 0,
  .gc_mark   = 1,
  .next_free = NULL,
  .ptr       = { .type = CELL_TYPE_SYMBOL, .symbol = AS_SYM ("t") },
};

PrimWrapper wrapper_nil = {
  .free      = 0,
  .gc_mark   = 1,
  .next_free = NULL,
  .ptr       = { .type = CELL_TYPE_NIL, .cons = { .car = NIL, .cdr = NIL } },
};

PrimWrapper wrapper_quote = {
  .free      = 0,
  .gc_mark   = 1,
  .next_free = NULL,
  .ptr       = { .type = CELL_TYPE_SYMBOL, .symbol = AS_SYM ("quote") },
};

// Only the list operations needed by VM bookkeeping remain native. The
// public list library is implemented by ordinary Scheme prelude procedures.
bool
cell_is_proper_list (const Cell *list)
{
  const Cell *slow = list;
  const Cell *fast = list;
  while (cell_is_pair (fast))
    {
      fast = CDR (fast);
      if (cell_is_nil (fast))
        return true;
      if (!cell_is_pair (fast))
        return false;
      fast = CDR (fast);
      slow = CDR (slow);
      if (fast == slow)
        return false;
    }
  return cell_is_nil (fast);
}

size_t
length (Cell *list)
{
  if (!cell_is_proper_list (list))
    return 0;
  size_t count = 0;
  for (Cell *p = list; cell_is_pair (p); p = CDR (p))
    ++count;
  return count;
}

Cell *
reverse_inplace (Cell *list)
{
  if (!cell_is_proper_list (list))
    return NIL;
  Cell *previous = NIL;
  while (cell_is_pair (list))
    {
      Cell *next = CDR (list);
      cell_set_cdr (list, previous);
      previous = list;
      list     = next;
    }
  return previous;
}
