#ifndef TYPES_H
#define TYPES_H

#include <stdbool.h>
#include <stddef.h>

#include "dict.h"

typedef struct cell Cell;
typedef struct lm   LM;

typedef enum
{
  CELL_TYPE_NIL,
  CELL_TYPE_INTEGER,
  CELL_TYPE_FLOAT,
  CELL_TYPE_SYMBOL,
  CELL_TYPE_STRING,
  CELL_TYPE_CONS,
  CELL_TYPE_BUILTIN,
  CELL_TYPE_LAMBDA,
  CELL_TYPE_ENV,
  CELL_TYPE_UNKNOWN,
  CELL_TYPE_COUNT
} CellType;

typedef long long Integer;
typedef double    Float;

typedef struct
{
  const char *str;
  size_t      len;
} Symbol;

typedef struct
{
  Cell *car;
  Cell *cdr;
} Cons;

typedef enum
{
#define X(id, is_special_form, arity, function) BUILTIN_##id,
#include "builtins.def"
#undef X
  BUILTIN_COUNT
} BuiltinId;

typedef struct
{
  Cell *params;
  Cell *body;
  Cell *env;
} Lambda;

typedef struct
{
  Cell *parent;
  Dict *dict;
} Env;

struct cell
{
  CellType type;
  union
  {
    Integer   integer;
    Float     floating;
    char     *string;
    Symbol    symbol;
    Cons      cons;
    BuiltinId builtin;
    Lambda    lambda;
    Env       env;
  };
};

// Typed constructors consistently take the owning VM first. Unlike a variadic
// constructor, their argument types and counts are checked by the compiler.
Cell *cell_make_integer (LM *lm, Integer value);
Cell *cell_make_float (LM *lm, Float value);
// Symbol text is borrowed (normally from str_intern) and must outlive the
// cell.
Cell *cell_make_symbol (LM *lm, const char *text, size_t length);
Cell *cell_make_pair (LM *lm, Cell *car, Cell *cdr);
Cell *cell_make_lambda (LM *lm, Cell *parameters, Cell *body);
Cell *cell_make_environment (LM *lm, Cell *parent);
// copy owns a duplicate; take assumes ownership of a malloc-compatible buffer.
Cell *cell_make_string_copy (LM *lm, const char *text);
Cell *cell_make_string_take (LM *lm, char *text);
void  cell_dispose (Cell *cell);

static inline bool
cell_has_type (const Cell *cell, CellType type)
{
  return cell && cell->type == type;
}

static inline bool
cell_is_pair (const Cell *cell)
{
  return cell_has_type (cell, CELL_TYPE_CONS);
}

typedef struct
{
  Cell *cur;
} ConsIter;

static inline ConsIter
cons_iter (Cell *cell)
{
  return (ConsIter){ .cur = cell };
}

Cell *cons_next (ConsIter *iter);

#endif
