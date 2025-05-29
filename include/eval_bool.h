#ifndef EVAL_BOOL_H
#define EVAL_BOOL_H

#include "eval.h"

Cell *eval_eq (Cell *args, Context *ctx);
Cell *eval_not (Cell *args, Context *ctx);

#endif
