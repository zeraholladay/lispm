#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "list.h"
#include "safe_str.h"
#include "types.h"

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
list_eq (Cell *self, Cell *other)
{
  return type_eq (self, other)
         && ((IS_NIL (self) && IS_NIL (other)) || &self->cons == &other->cons);
}

static int
builtin_fn_eq (Cell *self, Cell *other)
{
  return type_eq (self, other) && &self->builtin_fn == &other->builtin_fn;
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
  [TYPE_NIL] = { .type_name = "NIL", .eq_fn = nil_eq },
  [TYPE_INTEGER] = { .type_name = "INTEGER", .eq_fn = integer_eq },
  [TYPE_STRING] = { .type_name = "STRING", .eq_fn = string_eq },
  [TYPE_SYMBOL] = { .type_name = "SYMBOL", .eq_fn = symbol_eq },
  [TYPE_CONS] = { .type_name = "CONS", .eq_fn = list_eq },
  [TYPE_BUILTIN_FN] = { .type_name = "BUILTIN", .eq_fn = builtin_fn_eq },
  [TYPE_LAMBDA] = { .type_name = "LAMBDA", .eq_fn = lambda_eq },
};

const Type *
type (Cell *self)
{
  if (!self || IS_NIL (self))
    return &type_tab[TYPE_NIL];
  return &type_tab[self->type];
}

Cell *
cons_lambda (Pool **p, Cell *params, Cell *body)
{
  Cell *node = pool_xalloc_hier (p);
  node->type = TYPE_LAMBDA;
  node->lambda.params = params;
  node->lambda.body = body;
  return node;
}

Cell *
cons_integer (Pool **p, Integer i)
{
  Cell *node = pool_xalloc_hier (p);
  node->type = TYPE_INTEGER;
  node->integer = i;
  return node;
}

Cell *
cons_cons (Pool **p, Cell *car, Cell *cdr)
{
  Cell *node = pool_xalloc_hier (p);
  node->type = TYPE_CONS;
  CAR (node) = car;
  CDR (node) = cdr;
  return node;
}

Cell *
cons_string (Pool **p, char *str)
{
  Cell *node = pool_xalloc_hier (p);
  node->type = TYPE_STRING;
  node->string = str;
  return node;
}

Cell *
cons_symbol (Pool **p, const char *str, size_t len)
{
  Cell *node = pool_xalloc_hier (p);
  node->type = TYPE_SYMBOL;
  node->symbol.str = str;
  node->symbol.len = len;
  return node;
}
