#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "err.h"
#include "keywords.h"
#include "types.h"
#include "utils.h"

static inline int
type_eq (Cell *self, Cell *other)
{
  return type (self) == type (other);
}

static int
nil_eq (Cell *self, Cell *other)
{
  return self == other;
}

static int
integer_eq (Cell *self, Cell *other)
{
  return type_eq (self, other) && self->integer == other->integer;
}

static int
symbol_eq (Cell *self, Cell *other)
{
  return type_eq (self, other) && self->symbol.str == other->symbol.str;
}

static int
cons_eq (Cell *self, Cell *other)
{
  return type_eq (self, other)
         && ((NILP (self) && NILP (other)) || &self->cons == &other->cons);
}

static int
thunk_eq (Cell *self, Cell *other)
{
  return type_eq (self, other) && &self->thunk == &other->thunk;
}

static int
lambda_eq (Cell *self, Cell *other)
{
  return type_eq (self, other) && &self->lambda == &other->lambda;
}

static int
string_eq (Cell *self, Cell *other)
{
  return type_eq (self, other) && (!strcmp (self->string, other->string));
}

static Type type_tab[] = {
  [TYPE_NIL] = { .type_name = "NIL", .eq = nil_eq },
  [TYPE_INTEGER] = { .type_name = "INTEGER", .eq = integer_eq },
  [TYPE_STRING] = { .type_name = "STRING", .eq = string_eq },
  [TYPE_SYMBOL] = { .type_name = "SYMBOL", .eq = symbol_eq },
  [TYPE_CONS] = { .type_name = "CONS", .eq = cons_eq },
  [TYPE_THUNK] = { .type_name = "THUNK", .eq = thunk_eq },
  [TYPE_LAMBDA] = { .type_name = "LAMBDA", .eq = lambda_eq },
};

const Type *
type (Cell *self)
{
  if (!self || NILP (self))
    return &type_tab[TYPE_NIL];
  return &type_tab[self->type];
}

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
      c->lambda.body = va_arg (ap, Cell *);
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
    case TYPE_ERROR:
      c->error.err_code = va_arg (ap, ErrorCode);
      c->error.cell = va_arg (ap, Cell *);
      break;
    default:
      return NIL;
      break;
    }
  va_end (ap);
  return c;
}
