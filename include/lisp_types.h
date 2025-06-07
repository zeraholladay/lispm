#ifndef LISP_TYPES_H
#define LISP_TYPES_H

#include <stdarg.h>
#include <stddef.h>

#include "lisp_err.h"
#include "lisp_mach.h"

#define IS_INST(ptr, x) ((ptr) && (ptr)->type == TYPE_##x)

#define INTEGER(integer, lm) (new (lm, TYPE_INTEGER, integer))
#define SYMBOL(str, len, lm) (new (lm, TYPE_SYMBOL, str, len))
#define STRING(str, lm) (new (lm, TYPE_STRING, str))
#define CONS(car, cdr, lm) (new (lm, TYPE_CONS, car, cdr))
#define LAMBDA(params, body, lm) (new (lm, TYPE_LAMBDA, params, body))
#define ERROR(err_code, msg, lm)                                              \
  (new (lm, TYPE_ERROR, err_code, STRING (msg, lm)))

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

typedef struct Cell *(*Fn) (LM *lm, struct Cell *);

typedef struct
{
  const char *name;
  int is_lispm, arity;
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
Cell *new (LM *lm, TypeEnum type, ...);

#endif
