#ifndef TYPES_H
#define TYPES_H

#include <limits.h>
#include <stddef.h>

#include "context.h"
#include "palloc.h"

#define IS_TYPE(nptr, kind) ((nptr) != NULL && (nptr)->type == (kind))

#define IS_SYMBOL(nptr) IS_TYPE ((nptr), TYPE_SYMBOL)
#define IS_INTEGER(nptr) IS_TYPE ((nptr), TYPE_INTEGER)
#define IS_STRING(nptr) IS_TYPE ((nptr), TYPE_STRING)
#define IS_CONS(nptr) IS_TYPE ((nptr), TYPE_CONS)
#define IS_BUILTIN_FN(nptr) IS_TYPE ((nptr), TYPE_BUILTIN_FN)
#define IS_SPECIAL_FORM(nptr)                                                 \
  (IS_TYPE ((nptr), TYPE_BUILTIN_FN) && GET_BUILTIN_FN (nptr)->is_sf)
#define IS_LAMBDA(nptr) IS_TYPE ((nptr), TYPE_LAMBDA)
#define GET_SYMBOL(nptr) ((nptr)->symbol)
#define GET_INTEGER(nptr) ((nptr)->integer)
#define GET_STRING(nptr) ((nptr)->string)
#define GET_CONS(nptr) (&(nptr)->cons)
#define GET_BUILTIN_FN(nptr) ((nptr)->builtin_fn)
#define GET_LAMBDA(nptr) (&(nptr)->lambda)
#define GET_LAMBDA_PARAMS(nptr) ((nptr)->lambda.params)
#define GET_LAMBDA_BODY(nptr) ((nptr)->lambda.body)

struct Node;
typedef struct Node Node;

typedef char *(*StrFn) (Node *);
typedef int (*EqFn) (Node *, Node *);

typedef struct Type
{
  const char *type_name;
  StrFn str_fn;
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
  TYPE_LAMBDA      // user-defined fn
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
  int is_sf, arity;
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
