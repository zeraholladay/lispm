#ifndef _PARSER_H_
#define _PARSER_H_

bool parser_buf (const char *input, Node **ast_head, Context *ctx);
bool parser_stream (FILE *restrict in, Node **ast_head, Context *ctx);

#endif
