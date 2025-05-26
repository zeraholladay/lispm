#ifndef REPL_H
#define REPL_H

#include "eval_ctx.h"

void lispm_init (Context *ctx);
void lispm_destroy (Context *ctx);
int lispm_eval_progn (Context *ctx);
int lispm_repl (Context *ctx);
int lispm_main (int argc, char **argv);

#endif