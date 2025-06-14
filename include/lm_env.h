#ifndef LM_ENV_H
#define LM_ENV_H

#include <stdbool.h>

#include "lm_secd.h"
#include "types.h"

bool lm_env_define (LM *lm, Cell *car, Cell *cdr);
bool lm_env_set (LM *lm, Cell *car, Cell *cdr);
Cell *lm_env_lookup (LM *lm, Cell *sym);

#endif
