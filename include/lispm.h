#ifndef LISPM_H
#define LISPM_H

#include "context.h"
#include "eval.h"

#ifndef LISPM_STK_MAX
#define LISPM_STK_MAX 1024
#endif

#ifndef LISPM_CTL_MAX
#define LISPM_CTL_MAX 512
#endif

#ifndef LM_OBJ_POOL_CAP
#define LM_OBJ_POOL_CAP 1024
#endif

typedef struct lispm_secd LM;

// inst
LM *lm_create (void);
void lm_destroy (LM *lm);

// create a new cell/type
Cell *lm_alloc_cell (LM *lm);

// env
Cell *lm_env_lkup (LM *lm, const char *key);
bool lm_env_let (LM *lm, const char *key, Cell *val);
bool lm_env_set (LM *lm, const char *key, Cell *val);

// execute progn
Cell *lispm_progn (Cell *progn, Context *ctx);

#endif
