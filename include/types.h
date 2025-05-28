#ifndef TYPES_H
#define TYPES_H

#include <limits.h>
#include <stddef.h>

#include "context.h"
#include "palloc.h"

#define IS(ptr, T) ((ptr) && (ptr)->type == TYPE_##T)

struct Node;
typedef struct Node Node;

typedef int (*EqFn) (Node *, Node *);

typedef struct Type
{
  const char *type_name;
  EqFn eq_fn;
} Type;

// Nodes
typedef enum
{
  TYPE_NIL,        // special constant
  TYPE_SYMBOL,     // identifiers
  TYPE_INTEGER,    // literal
  TYPE_STRING,     // literal
  TYPE_CONS,       // cons cells
  TYPE_BUILTIN_FN, // builtin fn
  TYPE_LAMBDA,     // user-defined fn
  TYPE_UNKNOWN,    // unknown ptr
  _TYPE_END        // entinel for end
} TypeEnum;

typedef long long Integer;

typedef struct
{
  const char *str;
  size_t len;
} Symbol;

typedef struct
{
  Node *car; // Contents of the Address Register
  Node *cdr; // Contents of the Decrement Register
} Cons;

typedef struct Node *(*Fn) (struct Node *, struct Context *);

typedef struct
{
  const char *name;
  int sform, arity;
  Fn fn;
} BuiltinFn;

typedef struct
{
  Node *params;
  Node *body;
} Lambda;

struct Node
{
  TypeEnum type;
  union
  {
    // Literal values
    Integer integer;
    char *string;
    Symbol symbol;
    // Composite structures
    Cons cons;
    // Function-like values
    const BuiltinFn *builtin_fn;
    Lambda lambda;
  };
};

const Type *type (Node *self);
Node *cons_lambda (Pool **p, Node *params, Node *body);
Node *cons_integer (Pool **p, Integer i);
Node *cons_cons (Pool **p, Node *car, Node *cdr);
Node *cons_string (Pool **p, char *str);
Node *cons_symbol (Pool **p, const char *str, size_t len);

#endif
