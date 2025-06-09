#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>

#include "lm.h"
#include "types.h"

bool parser_buf (const char *input, Cell **ast_head, LM *lm);
bool parser_stream (FILE *instrm, Cell **ast_head, LM *lm);

#endif
