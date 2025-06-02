#ifndef REPL_H
#define REPL_H

#include "lispm.h"

void lispm_init (LM *lm);
void lispm_destroy (LM *lm);
int lispm_eval_progn (LM *lm);
int lispm_repl (LM *lm);
int lispm_main (int argc, char **argv);

#endif