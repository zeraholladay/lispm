#ifndef THUNKS_H
#define THUNKS_H

#include <stdbool.h>

#include "types.h"

static Cell thunk_cells[_THUNK_END] = {
#define X(sym, is_l, ar, fn)                                                  \
  [THUNK_##sym] = { .type = TYPE_THUNK, .thunk = THUNK_##sym },
#include "thunks.def"
#undef X
};

Cell *thunker (LM *lm, Cell *fn, Cell *arglist);
const char *thunk_get_name (Cell *thunk);
bool thunk_is_lispm (Cell *c);

#endif
