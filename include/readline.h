#ifndef READLINE_H
#define READLINE_H

#include <stdlib.h>

void rl_init (void);
// Caller owns the returned line; NULL means EOF.
char *rl_readline (void);

#endif
