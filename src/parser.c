#include <stdbool.h>
#include <stdio.h>

#include "bison.h"
#include "flex.h"
#include "lispm.h"
#include "types.h"

bool
parser_buf (const char *input, Cell **ast_head, LM *lm)
{
  yy_scan_string (input);

  int status = yyparse (ast_head, lm);

  yylex_destroy ();

  return (status == 0);
}

bool
parser_stream (FILE *instrm, Cell **ast_head, LM *lm)
{
  yyset_in (instrm);

  int status = yyparse (ast_head, lm);

  return (status == 0);
}
