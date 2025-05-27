#ifndef EVAL_MATH_H
#define EVAL_MATH_H

#include "eval.h"

Node *eval_gt (Node *args, Ctx *ctx);
Node *eval_lt (Node *args, Ctx *ctx);
Node *eval_add (Node *args, Ctx *ctx);
Node *eval_sub (Node *args, Ctx *ctx);
Node *eval_mul (Node *args, Ctx *ctx);
Node *eval_div (Node *args, Ctx *ctx);

#endif
