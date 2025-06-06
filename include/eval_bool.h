#ifndef EVAL_BOOL_H
#define EVAL_BOOL_H

#include "eval.h"

Cell *eval_eq (Cell *args, LM *lm);
Cell *eval_not (Cell *args, LM *lm);

#endif
