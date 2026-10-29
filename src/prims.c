#include "prims.h"

#define AS_SYM(name) { .str = name, .len = sizeof (name) - 1 }

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

// Only the list operations needed by VM bookkeeping remain native. The
// public list library is implemented by ordinary Scheme prelude procedures.
bool
proper_list_p (Cell *list)
{
  Cell *slow = list;
  Cell *fast = list;
  while (CONSP (fast))
    {
      fast = CDR (fast);
      if (NILP (fast))
        return true;
      if (!CONSP (fast))
        return false;
      fast = CDR (fast);
      slow = CDR (slow);
      if (fast == slow)
        return false;
    }
  return NILP (fast);
}

size_t
length (Cell *list)
{
  if (!proper_list_p (list))
    return 0;
  size_t count = 0;
  for (Cell *p = list; CONSP (p); p = CDR (p))
    ++count;
  return count;
}

Cell *
reverse_inplace (Cell *list)
{
  if (!proper_list_p (list))
    return NIL;
  Cell *previous = NIL;
  while (CONSP (list))
    {
      Cell *next = CDR (list);
      RPLACD (list, previous);
      previous = list;
      list     = next;
    }
  return previous;
}
