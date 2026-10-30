#include <stdlib.h>
#include <string.h>

#include "lm.h"
#include "prims.h"
#include "types.h"
#include "xalloc.h"

static Cell *
cell_allocate (LM *lm, CellType type)
{
  Cell *cell = lm_alloc_cell (lm);
  cell->type = type;
  return cell;
}

Cell *
cell_make_integer (LM *lm, Integer value)
{
  Cell *cell    = cell_allocate (lm, CELL_TYPE_INTEGER);
  cell->integer = value;
  return cell;
}

Cell *
cell_make_float (LM *lm, Float value)
{
  Cell *cell     = cell_allocate (lm, CELL_TYPE_FLOAT);
  cell->floating = value;
  return cell;
}

Cell *
cell_make_symbol (LM *lm, const char *text, size_t length)
{
  Cell *cell   = cell_allocate (lm, CELL_TYPE_SYMBOL);
  cell->symbol = (Symbol){ .str = text, .len = length };
  return cell;
}

Cell *
cell_make_pair (LM *lm, Cell *car, Cell *cdr)
{
  Cell *cell = cell_allocate (lm, CELL_TYPE_CONS);
  cell->cons = (Cons){ .car = car, .cdr = cdr };
  return cell;
}

Cell *
cell_make_lambda (LM *lm, Cell *parameters, Cell *body)
{
  Cell *cell   = cell_allocate (lm, CELL_TYPE_LAMBDA);
  cell->lambda = (Lambda){ .params = parameters, .body = body, .env = NIL };
  return cell;
}

Cell *
cell_make_environment (LM *lm, Cell *parent)
{
  Cell *cell = cell_allocate (lm, CELL_TYPE_ENV);
  cell->env  = (Env){ .parent = parent, .dict = dict_create (NULL, 0) };
  return cell;
}

Cell *
cell_make_string_take (LM *lm, char *text)
{
  Cell *cell   = cell_allocate (lm, CELL_TYPE_STRING);
  cell->string = text;
  return cell;
}

Cell *
cell_make_string_copy (LM *lm, const char *text)
{
  size_t size = strlen (text) + 1;
  char  *copy = xmalloc (size);
  memcpy (copy, text, size);
  return cell_make_string_take (lm, copy);
}

void
cell_dispose (Cell *cell)
{
  if (cell->type == CELL_TYPE_ENV)
    dict_destroy (cell->env.dict);
  else if (cell->type == CELL_TYPE_STRING)
    free (cell->string);
}

Cell *
cons_next (ConsIter *iter)
{
  Cell *cell = iter->cur;
  if (cell != NIL)
    {
      iter->cur = CDR (cell);
      return CAR (cell);
    }
  return NULL;
}
