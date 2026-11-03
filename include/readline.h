#ifndef READLINE_H
#define READLINE_H

#include "lm.h"
#include <stdlib.h>

void rl_init (void);
// Caller owns the returned line; NULL means EOF.
char *rl_readline (LM *lm);
// Readline match array: common prefix first, then alternatives (if ambiguous).
// Caller frees each string and the array. NULL means no matching symbol.
char **rl_symbol_matches (LM *lm, const char *text);

#endif
