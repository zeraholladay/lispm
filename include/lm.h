#ifndef LM_H
#define LM_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>

#include "dict.h"
#include "palloc.h"

#ifndef LISPM_STK_MAX
#define LISPM_STK_MAX 4096
#endif

#ifndef LISPM_CTL_MAX
#define LISPM_CTL_MAX 4096
#endif

#ifndef LM_CAP_PER_POOL
#define LM_CAP_PER_POOL 4096
#endif

// forward decls
typedef struct cell Cell;
typedef struct lm   LM;

typedef enum
{
#define X(tag, ...) s_##tag,
#include "lm.def"
#undef X
  COUNT,
} StateEnum;

typedef union
{
#define X(tag, ...)                                                           \
  struct                                                                      \
  {                                                                           \
    __VA_ARGS__;                                                              \
  } tag;
#include "lm.def"
#undef X
} Union;

typedef struct state
{
  StateEnum s;
  Union     u;
} State;

typedef struct lm
{
  struct
  {
    size_t sp;
    Cell  *cells[LISPM_STK_MAX];
  } stk;
  struct
  {
    Cell *current;
    Cell *global;
  } env;
  struct
  {
    size_t sp;
    State  states[LISPM_CTL_MAX];
  } ctl;
  Pool *pool;
  bool  err_bool;
} LM;

LM  *lm_create (void);
void lm_destroy (LM *lm);

// create a new cell/type
Cell *lm_alloc_cell (LM *lm);

// external entrypoints
Cell *lm_funcall (LM *lm, Cell *fn, Cell *arglist);
Cell *lm_progn (LM *lm, Cell *progn);

#endif
