#ifndef EVAL_BOOL_H
#define EVAL_BOOL_H

#include "eval.h"

Node *eval_eq (Node *args, Ctx *ctx);
Node *eval_not (Node *args, Ctx *ctx);

#endif
