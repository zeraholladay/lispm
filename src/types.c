#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "list.h"
#include "safe_str.h"
#include "types.h"

static inline int
type_eq (Node *self, Node *other)
{
  return type (self) == type (other);
}

static int
nil_eq (Node *self, Node *other)
{
  return self == other;
}

static int
integer_eq (Node *self, Node *other)
{
  return type_eq (self, other) && self->integer == other->integer;
}

static int
symbol_eq (Node *self, Node *other)
{
  return type_eq (self, other) && self->symbol.str == other->symbol.str;
}

static int
list_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && ((IS_NIL (self) && IS_NIL (other))
             || &self->cons == &other->cons);
}

static int
primitive_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && &self->builtin_fn == &other->builtin_fn;
}

static int
lambda_eq (Node *self, Node *other)
{
  return type_eq (self, other) && &self->lambda == &other->lambda;
}

static int
string_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && (!strcmp (self->string, other->string));
}

static Type type_tab[] = {
  [TYPE_NIL] = { .type_name = "NIL", .eq_fn = nil_eq },
  [TYPE_INTEGER] = { .type_name = "INTEGER", .eq_fn = integer_eq },
  [TYPE_STRING] = { .type_name = "STRING", .eq_fn = string_eq },
  [TYPE_SYMBOL] = { .type_name = "SYMBOL", .eq_fn = symbol_eq },
  [TYPE_CONS] = { .type_name = "CONS", .eq_fn = list_eq },
  [TYPE_BUILTIN_FN] = { .type_name = "PRIMITIVE", .eq_fn = primitive_eq },
  [TYPE_LAMBDA] = { .type_name = "LAMBDA", .eq_fn = lambda_eq },
};

const Type *
type (Node *self)
{
  if (!self || IS_NIL (self))
    return &type_tab[TYPE_NIL];
  return &type_tab[self->type];
}

Node *
cons_lambda (Pool **p, Node *params, Node *body)
{
  Node *node = pool_xalloc_hier (p);
  node->type = TYPE_LAMBDA;
  node->lambda.params = params;
  node->lambda.body = body;
  return node;
}

Node *
cons_integer (Pool **p, Integer i)
{
  Node *node = pool_xalloc_hier (p);
  node->type = TYPE_INTEGER;
  node->integer = i;
  return node;
}

Node *
cons_cons (Pool **p, Node *car, Node *cdr)
{
  Node *node = pool_xalloc_hier (p);
  node->type = TYPE_CONS;
  CAR (node) = car;
  CDR (node) = cdr;
  return node;
}

Node *
cons_string (Pool **p, char *str)
{
  Node *node = pool_xalloc_hier (p);
  node->type = TYPE_STRING;
  node->string = str;
  return node;
}

Node *
cons_symbol (Pool **p, const char *str, size_t len)
{
  Node *node = pool_xalloc_hier (p);
  node->type = TYPE_SYMBOL;
  node->symbol.str = str;
  node->symbol.len = len;
  return node;
}
