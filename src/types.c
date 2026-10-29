#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "prims.h"
#include "types.h"
#include "xalloc.h"

Cell *
new (LM *lm, TypeEnum type, ...)
{
  va_list ap;
  va_start (ap, type);

  Cell *c = lm_alloc_cell (lm);
  c->type = type;

  switch (type)
    {
    case TYPE_LAMBDA:
      c->lambda.params = va_arg (ap, Cell *);
      c->lambda.body   = va_arg (ap, Cell *);
      c->lambda.env    = NIL;
      break;

    case TYPE_ENV:
      c->env.parent = va_arg (ap, Cell *);
      c->env.dict   = dict_create (NULL, 0);
      break;

    case TYPE_INTEGER:
      c->integer = va_arg (ap, Integer);
      break;

    case TYPE_FLOAT:
      c->_float = va_arg (ap, Float);
      break;

    case TYPE_CONS:
      c->cons.car = va_arg (ap, Cell *);
      c->cons.cdr = va_arg (ap, Cell *);
      break;

    case TYPE_STRING:
      {
        const char *string = va_arg (ap, const char *);
        size_t      len    = strlen (string) + 1;
        c->string          = xmalloc (len);
        memcpy (c->string, string, len);
      }
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
string_take (LM *lm, char *string)
{
  Cell *cell   = lm_alloc_cell (lm);
  cell->type   = TYPE_STRING;
  cell->string = string;
  return cell;
}

void
cell_dispose (Cell *cell)
{
  if (cell->type == TYPE_ENV)
    dict_destroy (cell->env.dict);
  else if (cell->type == TYPE_STRING)
    free (cell->string);
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
