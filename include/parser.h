#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>

#include "context.h"
#include "types.h"

bool parser_buf (const char *input, Cell **ast_head, Context *ctx);
bool parser_stream (FILE *instrm, Cell **ast_head, Context *ctx);

#endif
