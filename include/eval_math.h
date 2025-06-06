#ifndef EVAL_MATH_H
#define EVAL_MATH_H

#include "eval.h"

Cell *eval_gt (Cell *args, LM *lm);
Cell *eval_lt (Cell *args, LM *lm);
Cell *eval_add (Cell *args, LM *lm);
Cell *eval_sub (Cell *args, LM *lm);
Cell *eval_mul (Cell *args, LM *lm);
Cell *eval_div (Cell *args, LM *lm);

#endif
