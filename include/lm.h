#ifndef LM_H
#define LM_H

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
#define X(id, tag, ...) STATE_##id,
#include "lm.def"
#undef X
  STATE_KIND_COUNT,
} StateKind;

typedef union
{
#define X(id, tag, ...)                                                       \
  struct                                                                      \
  {                                                                           \
    __VA_ARGS__;                                                              \
  } tag;
#include "lm.def"
#undef X
} StatePayload;

// A pending VM instruction/continuation on the control stack: a tagged union.
// kind selects the operation and the matching payload member (for example,
// STATE_EVAL uses payload.eval). Only that member is meaningful for this
// state. The evaluator executes it; GC uses the same tag to trace its Cell
// pointers while it is pending. Use lm_states.h constructors to keep tag/data
// consistent.
typedef struct state
{
  StateKind    kind;
  StatePayload payload;
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
    // Peak pending-state count for the latest evaluation. Tail recursion
    // should leave this bounded as the number of iterations increases.
    size_t high_water;
    State  states[LISPM_CTL_MAX];
  } ctl;
  Pool *pool;
  bool  has_error;
} LM;

typedef struct
{
  bool  success;
  Cell *value;
} EvaluationResult;

// Creates a VM and loads the embedded Scheme prelude; NULL on bootstrap
// failure.
LM  *lm_create (void);
void lm_destroy (LM *lm);

// create a new cell/type
Cell *lm_alloc_cell (LM *lm);

// external entrypoints
// Checked evaluation distinguishes a successful NIL from a failed evaluation.
EvaluationResult lm_evaluate (LM *lm, Cell *forms);
// Value-only compatibility API: returns NIL after a failure.
Cell *lm_progn (LM *lm, Cell *progn);

#endif
