#ifndef _PARSER_H_
#define _PARSER_H_

#include <stdio.h>

#include "context.h"
#include "types.h"

bool parser_buf (const char *input, Node **ast_head, Context *ctx);
bool parser_stream (FILE *instrm, Node **ast_head, Context *ctx);

#endif
