#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "prims.h"
#include "types.h"

Cell *
new (LM *lm, TypeEnum type, ...)
{
  va_list ap;
  va_start (ap, type);

  Cell *c = lm_alloc_cell (lm);

  c->type = type;
  memset (&c->loc, 0, sizeof (c->loc));

  switch (type)
    {
    case TYPE_LAMBDA:
      c->lambda.params = va_arg (ap, Cell *);
      c->lambda.body   = va_arg (ap, Cell *);
      break;

    case TYPE_INTEGER:
      c->integer = va_arg (ap, Integer);
      break;

    case TYPE_CONS:
      c->cons.car = va_arg (ap, Cell *);
      c->cons.cdr = va_arg (ap, Cell *);
      break;

    case TYPE_STRING:
      c->string = va_arg (ap, char *);
      break;

    case TYPE_SYMBOL:
      c->symbol.str = va_arg (ap, const char *);
      c->symbol.len = va_arg (ap, size_t);
      break;

    default:
      return NIL;
      break;
    }

  va_end (ap);
  return c;
}

Cell *
cons_next (ConsIter *iter)
{
  Cell *cur = iter->cur;

  if (cur != NIL)
    {
      iter->cur = CDR (iter->cur);
      return CAR (cur);
    }

  return NULL;
}
