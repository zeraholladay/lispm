#ifndef CONTEXT_H
#define CONTEXT_H

#include "env.h"
#include "palloc.h"

#define CTX_POOL(ctx) ((ctx)->p)

typedef struct Context
{
  Pool *pool;
  Env *env;
} Context;

#endif
