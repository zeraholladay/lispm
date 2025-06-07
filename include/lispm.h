#ifndef LISPM_H
#define LISPM_H

#include <stdbool.h>

#ifndef LISPM_STK_MAX
#define LISPM_STK_MAX 1024
#endif

#ifndef LISPM_CTL_MAX
#define LISPM_CTL_MAX 512
#endif

#ifndef LISPM_DUMP_MAX
#define LISPM_DUMP_MAX 512
#endif

#ifndef LM_OBJ_POOL_CAP
#define LM_OBJ_POOL_CAP 1024
#endif

// forward decls
typedef struct Cell Cell;
typedef struct lispm_secd LM;

// inst
LM *lm_create (void);
void lm_destroy (LM *lm);

// create a new cell/type
Cell *lm_alloc_cell (LM *lm);

// external access to env
Cell *lm_env_lkup (LM *lm, const char *key);
bool lm_env_let (LM *lm, const char *key, Cell *val);
bool lm_env_set (LM *lm, const char *key, Cell *val);

// external entrypoints
Cell *lm_funcall (LM *lm, Cell *fn, Cell *arglist);
Cell *lm_progn (LM *lm, Cell *progn);

#endif
