#ifndef THUNKS_H
#define THUNKS_H

#include <stdbool.h>

#include "types.h"

Cell *thunker (LM *lm, Cell *fn, Cell *arglist);
const char *thunk_get_name (Cell *thunk);
bool thunk_is_lispm (Cell *c);

#endif
