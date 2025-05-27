#include <stdbool.h>
#include <stdio.h>

#include "bison.h"
#include "context.h"
#include "flex.h"
#include "types.h"

bool
parser_buf (const char *input, Node **ast_head, Context *ctx)
{
  yy_scan_string (input);

  // yypush_buffer_state (scan_buf);

  int status = yyparse (ast_head, ctx);

  yylex_destroy ();

  // yypop_buffer_state ();

  return (status == 0);
}

bool
parser_stream (FILE *restrict in, Node **ast_head, Context *ctx)
{
  yyset_in (in);

  int status = yyparse (ast_head, ctx);

  return (status == 0);
}
