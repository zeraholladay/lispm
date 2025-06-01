#ifndef LISPM_H
#define LISPM_H

#include "context.h"
#include "eval.h"

#define LISPM_STK_MAX 1024
#define LISPM_CTL_MAX 512

Cell *lispm_progn (Cell *progn, Context *ctx);

#endif
