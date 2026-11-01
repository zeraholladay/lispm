#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>

#include "lm.h"
#include "types.h"

bool parser_buf (const char *input, Cell **ast_head, LM *lm);
bool parser_stream (FILE *instrm, Cell **ast_head, LM *lm);

// Named variants include source_name in diagnostics. The original wrappers
// use <input>/<stream>; all calls reset location tracking to line 1, column 1.
// Streams are buffered (without seeking) to retain source lines for
// diagnostics.
bool parser_buf_named (const char *source_name, const char *input,
                       Cell **ast_head, LM *lm);
bool parser_stream_named (const char *source_name, FILE *input,
                          Cell **ast_head, LM *lm);

#endif
