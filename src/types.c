#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "list.h"
#include "safe_str.h"
#include "types.h"

// Type eq
static inline int
type_eq (Node *self, Node *other)
{
  return type (self) == type (other);
}

// NIL type
static int
nil_eq (Node *self, Node *other)
{
  (void)self;
  (void)other;
  return self == other;
}
// Integer type
static int
integer_eq (Node *self, Node *other)
{
  return type_eq (self, other) && GET_INTEGER (self) == GET_INTEGER (other);
}

// Symbol type
static int
symbol_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && GET_SYMBOL (self).str == GET_SYMBOL (other).str;
}

// List type
static int
list_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && ((IS_NIL (self) && IS_NIL (other))
             || GET_CONS (self) == GET_CONS (other));
}

// Primitive type
static int
primitive_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && &GET_BUILTIN_FN (self) == &GET_BUILTIN_FN (other);
}

// Lambda type
static int
lambda_eq (Node *self, Node *other)
{
  return type_eq (self, other) && GET_LAMBDA (self) == GET_LAMBDA (other);
}

// String type
static int
string_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && (!strcmp (GET_STRING (self),
                      GET_STRING (other))); // FIX ME: strings should have len
}

static Type type_tab[] = {
  // Special constant
  [TYPE_NIL] = { .type_name = "NIL", .eq_fn = nil_eq },

  // Literal values
  [TYPE_INTEGER] = { .type_name = "INTEGER", .eq_fn = integer_eq },
  [TYPE_STRING] = { .type_name = "STRING", .eq_fn = string_eq },
  [TYPE_SYMBOL] = { .type_name = "SYMBOL", .eq_fn = symbol_eq },

  // Composite structures
  [TYPE_CONS] = { .type_name = "CONS", .eq_fn = list_eq },

  // Function-like values
  [TYPE_BUILTIN_FN] = { .type_name = "PRIMITIVE", .eq_fn = primitive_eq },
  [TYPE_LAMBDA] = { .type_name = "LAMBDA", .eq_fn = lambda_eq },
};

// type()
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
