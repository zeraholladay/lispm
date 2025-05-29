#ifndef TYPES_H
#define TYPES_H

#include <stdarg.h>
#include <stddef.h>

#include "context.h"
#include "palloc.h"

#define IS(ptr, x) ((ptr) && (ptr)->type == TYPE_##x)
#define IS_NOT(ptr, x) (!(IS (ptr, x)))

#define INTEGER(ctx, str, integer) (new (&(ctx)->p, TYPE_INTEGER, integer))
#define SYMBOL(ctx, str, len, ctx) (new (&(ctx)->p, TYPE_SYMBOL, str, len))
#define STRING(str) (new (&(ctx)->p, TYPE_STRING, str))
#define CONS(car, cdr, ctx) (new (&(ctx)->p, TYPE_CONS, car, cdr))
#define LAMBDA(car, params, body, ctx) (new (&(ctx)->p, TYPE_LAMBDA, params, body))

struct Cell;
typedef struct Cell Cell;

typedef int (*EqFn) (Cell *, Cell *);

typedef struct Type
{
  const char *type_name;
  EqFn eq;
} Type;

// Cells
typedef enum
{
  TYPE_NIL,        // special constant
  TYPE_INTEGER,    // literal
  TYPE_SYMBOL,     // identifiers
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
  Cell *car; // Contents of the Address Register
  Cell *cdr; // Contents of the Decrement Register
} Cons;

typedef struct Cell *(*Fn) (struct Cell *, struct Context *);

typedef struct
{
  const char *name;
  int sform, arity;
  Fn fn;
} BuiltinFn;

typedef struct
{
  Cell *params;
  Cell *body;
} Lambda;

struct Cell
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

const Type *type (Cell *self);
Cell *new (Pool **p, TypeEnum type, ...);

#endif
