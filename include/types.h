#ifndef TYPES_H
#define TYPES_H

#include <limits.h>
#include <stddef.h>

#include "env.h"
#include "eval_ctx.h"
#include "palloc.h"

#define IS_TYPE(node, kind) ((node) != NULL && (node)->type == (kind))

#define IS_SYMBOL(node) IS_TYPE ((node), TYPE_SYMBOL)
#define IS_INTEGER(node) IS_TYPE ((node), TYPE_INTEGER)
#define IS_STRING(node) IS_TYPE ((node), TYPE_STRING)
#define IS_LIST(node) IS_TYPE ((node), TYPE_LIST)
#define IS_BUILTIN_FN(node) IS_TYPE ((node), TYPE_BUILTIN_FN)
#define IS_SPECIAL_FORM(node)                                                 \
  (IS_TYPE ((node), TYPE_BUILTIN_FN) && GET_BUILTIN_FN (node)->is_sf)
#define IS_LAMBDA(node) IS_TYPE ((node), TYPE_LAMBDA)
#define GET_SYMBOL(node) ((node)->as.symbol)
#define GET_INTEGER(node) ((node)->as.integer)
#define GET_STRING(node) ((node)->as.string)
#define GET_LIST(node) (&(node)->as.list)
#define GET_BUILTIN_FN(node) ((node)->as.builtin_fn)
#define GET_LAMBDA(node) (&(node)->as.lambda)
#define GET_LAMBDA_PARAMS(node) ((node)->as.lambda.params)
#define GET_LAMBDA_BODY(node) ((node)->as.lambda.body)
#define GET_LAMBDA_ENV(node) ((node)->as.lambda.env)

struct Node;
typedef struct Node Node;

// Node type "object"
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
  TYPE_LIST,       // cons cells
  TYPE_BUILTIN_FN, // builtin fn
  TYPE_LAMBDA      // user-defined fn
} TypeEnum;

typedef long long Integer;

typedef struct
{
  const char *str;
  size_t len;
} Symbol;

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
  Env *env;
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
    struct
    {
      Node *first;
      Node *rest;
    } list;

    // Function-like values
    const BuiltinFn *builtin_fn;
    Lambda lambda;
  } as;
};

const Type *type (Node *self);
Node *cons_lambda (Pool **p, Node *params, Node *body, Env *env);
Node *cons_integer (Pool **p, Integer i);
Node *cons_list (Pool **p, Node *car, Node *cdr);
Node *cons_string (Pool **p, char *str);
Node *cons_symbol (Pool **p, const char *str, size_t len);

#endif
