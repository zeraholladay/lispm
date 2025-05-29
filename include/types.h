#ifndef TYPES_H
#define TYPES_H

#include <stdarg.h>
#include <stddef.h>

#include "context.h"
#include "error.h"
#include "palloc.h"

#define IS(ptr, x) ((ptr) && (ptr)->type == TYPE_##x)
#define IS_NOT(ptr, x) (!(IS (ptr, x)))

#define INTEGER(integer, ctx) (new (&(ctx)->pool, TYPE_INTEGER, integer))
#define SYMBOL(str, len, ctx) (new (&(ctx)->pool, TYPE_SYMBOL, str, len))
#define STRING(str, ctx) (new (&(ctx)->pool, TYPE_STRING, str))
#define CONS(car, cdr, ctx) (new (&(ctx)->pool, TYPE_CONS, car, cdr))
#define LAMBDA(params, body, ctx)                                             \
  (new (&(ctx)->pool, TYPE_LAMBDA, params, body))
#define ERROR(err_code, msg, ctx)                                             \
  (new (&(ctx)->pool, TYPE_ERROR, err_code, STRING (msg, ctx)))

// forward decls
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
  TYPE_ERROR,      // error
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

typedef struct
{
  ErrorCode err_code;
  Cell *cell;
} Error;

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
    // Error
    Error error;
  };
};

const Type *type (Cell *self);
Cell *new (Pool **p, TypeEnum type, ...);

#endif
