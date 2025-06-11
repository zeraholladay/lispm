#ifndef TYPES_H
#define TYPES_H

#include <stdarg.h>
#include <stddef.h>

#include "lm.h"
#include "lm_err.h"

#define IS_INST(ptr, x) ((ptr) && (ptr)->type == TYPE_##x)

#define INTEGER(integer, lm) (new (lm, TYPE_INTEGER, integer))
#define SYMBOL(str, len, lm) (new (lm, TYPE_SYMBOL, str, len))
#define STRING(str, lm) (new (lm, TYPE_STRING, str))
#define CONS(car, cdr, lm) (new (lm, TYPE_CONS, car, cdr))
#define LAMBDA(params, body, lm) (new (lm, TYPE_LAMBDA, params, body))

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
  TYPE_NIL,     // special constant
  TYPE_INTEGER, // literal
  TYPE_SYMBOL,  // identifiers
  TYPE_STRING,  // literal
  TYPE_CONS,    // cons cells
  TYPE_THUNK,   // thunk fn
  TYPE_LAMBDA,  // user-defined fn
  TYPE_UNKNOWN, // unknown ptr
  _TYPE_END     // sentinel for end
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

typedef enum
{
#define X(sym, is_l, ar, fn) THUNK_##sym,
#include "thunks.def"
#undef X
  _THUNK_END // sentinel for end
} ThunkEnum;

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
    ThunkEnum thunk;
    Lambda lambda;
  };
};

const Type *type (Cell *self);
Cell *new (LM *lm, TypeEnum type, ...);

#endif
