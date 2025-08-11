#ifndef TYPES_H
#define TYPES_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>

#include "lm.h"
#include "lm_err.h"

#define IS_INST(ptr, x) ((ptr) && (ptr)->type == TYPE_##x)

#define INTEGER(integer, lm)     (new (lm, TYPE_INTEGER, integer))
#define SYMBOL(str, len, lm)     (new (lm, TYPE_SYMBOL, str, len))
#define STRING(str, lm)          (new (lm, TYPE_STRING, str))
#define CONS(car, cdr, lm)       (new (lm, TYPE_CONS, car, cdr))
#define LAMBDA(params, body, lm) (new (lm, TYPE_LAMBDA, params, body))
#define CONTINUATION(stk, env, ctl, dmp, lm) \
  (new (lm, TYPE_CONTINUATION, stk, env, ctl, dmp))

// forward decls
typedef struct cell Cell;

// Cells
typedef enum
{
  TYPE_NIL,     // special constant
  TYPE_INTEGER, // literal number
  TYPE_SYMBOL,  // identifiers
  TYPE_STRING,  // literal string
  TYPE_CONS,    // cons cells
  TYPE_THUNK,   // thunk fn
  TYPE_LAMBDA,  // user-defined fn
  TYPE_CONTINUATION, // captured continuation
  TYPE_UNKNOWN, // unknown ptr
  _TYPE_END     // sentinel for end
} TypeEnum;

typedef long long Integer;

typedef struct
{
  const char *str;
  size_t      len;
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
  size_t stk_sp;
  size_t env_sp;
  size_t ctl_sp;
  size_t dmp_sp;
} Continuation;

struct cell
{
  TypeEnum type;
  union
  {
    // Literal values
    Integer integer;
    char   *string;
    Symbol  symbol;
    // Composite structures
    Cons cons;
    // Function-like values
    ThunkEnum thunk;
    Lambda    lambda;
    Continuation continuation;
  };
};

// cons iter
typedef struct
{
  Cell *cur;
} ConsIter;

static inline ConsIter
cons_iter (Cell *c)
{
  return (ConsIter){ .cur = c };
}

Cell *new (LM *lm, TypeEnum type, ...);
Cell *cons_next (ConsIter *iter);

#endif
