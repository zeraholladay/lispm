#ifndef TYPES_H
#define TYPES_H

#include <limits.h>
#include <stddef.h>

#include "context.h"
#include "palloc.h"

#define IS(ptr, T) ((ptr) && (ptr)->type == TYPE_##T)

struct Cell;
typedef struct Cell Cell;

typedef int (*EqFn) (Cell *, Cell *);

typedef struct Type
{
  const char *type_name;
  EqFn eq_fn;
} Type;

// Cells
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
Cell *cons_lambda (Pool **p, Cell *params, Cell *body);
Cell *cons_integer (Pool **p, Integer i);
Cell *cons_cons (Pool **p, Cell *car, Cell *cdr);
Cell *cons_string (Pool **p, char *str);
Cell *cons_symbol (Pool **p, const char *str, size_t len);

#endif
