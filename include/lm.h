#ifndef LM_H
#define LM_H

#include <stdarg.h>
#include <stdbool.h>

#include "lm_err.h"

#define LM_ERR_RET(lm, err_code, fmt, ...)                                    \
  do                                                                          \
    {                                                                         \
      lm_err_set ((lm), (err_code), (fmt), ##__VA_ARGS__);                    \
      return NIL;                                                             \
    }                                                                         \
  while (0)

// forward decls
typedef struct Cell Cell;
typedef struct lispm_secd LM;

// inst
LM *lm_create (void);
void lm_destroy (LM *lm);

// create a new cell/type
Cell *lm_alloc_cell (LM *lm);

// error access
bool lm_err_set (LM *lm, ErrorCode code, const char *fmt, ...);

// external entrypoints
Cell *lm_funcall (LM *lm, Cell *fn, Cell *arglist);
Cell *lm_progn (LM *lm, Cell *progn);

#endif
