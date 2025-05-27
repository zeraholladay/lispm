#ifndef REPL_H
#define REPL_H

#include "ctx.h"

void lispm_init (Ctx **ctx);
void lispm_destroy (Ctx *ctx);
int lispm_eval_progn (Ctx *ctx);
int lispm_repl (Ctx *ctx);
int lispm_main (int argc, char **argv);

#endif