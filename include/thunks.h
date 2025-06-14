#ifndef THUNKS_H
#define THUNKS_H

#include <stdbool.h>

#include "palloc.h"
#include "types.h"

typedef struct thunk_wrapper ThunkWrapper;

struct thunk_wrapper
{
  PALLOC_WRAPPER_FIELDS (ThunkWrapper)
  Cell ptr;
};

static ThunkWrapper wrapped_thunks[_THUNK_END] = {
#define X(sym, is_l, ar, fn)                                                  \
  [THUNK_##sym] = { .free = 0,                                                \
                    .gc_mark = 1,                                             \
                    .next_free = NULL,                                        \
                    .ptr = { .type = TYPE_THUNK, .thunk = THUNK_##sym } },
#include "thunks.def"
#undef X
};

Cell *thunker (LM *lm, Cell *fn, Cell *arglist);
const char *thunk_get_name (Cell *thunk);
bool thunk_is_lispm (Cell *c);

#endif
