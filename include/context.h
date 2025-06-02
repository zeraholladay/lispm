#ifndef CONTEXT_H
#define CONTEXT_H

#include "env.h"
#include "palloc.h"

typedef struct Context
{
  Pool *pool;
  Env *env;
} Context;

#endif
