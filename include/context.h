#ifndef CONTEXT_H
#define CONTEXT_H

#include "env.h"
#include "palloc.h"
#include "rb_tree.h"

#define CTX_POOL(ctx) ((ctx)->node_pool)

typedef struct Context
{
  Pool *node_pool;
  Env *env;
} Context;

#endif
