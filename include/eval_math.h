#ifndef EVAL_MATH_H
#define EVAL_MATH_H

#include "eval.h"

Cell *eval_gt (Cell *args, Context *ctx);
Cell *eval_lt (Cell *args, Context *ctx);
Cell *eval_add (Cell *args, Context *ctx);
Cell *eval_sub (Cell *args, Context *ctx);
Cell *eval_mul (Cell *args, Context *ctx);
Cell *eval_div (Cell *args, Context *ctx);

#endif
