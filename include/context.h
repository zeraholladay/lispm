#ifndef CONTEXT_H
#define CONTEXT_H

#include "env.h"
#include "palloc.h"

typedef struct Context
{
  Pool *p;
  Env *env;
} Context;

#endif
