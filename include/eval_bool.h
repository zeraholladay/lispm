#ifndef EVAL_BOOL_H
#define EVAL_BOOL_H

#include "eval.h"

Node *eval_eq (Node *args, Context *ctx);
Node *eval_and (Node *args, Context *ctx);
Node *eval_not (Node *args, Context *ctx);
Node *eval_or (Node *args, Context *ctx);

#endif
