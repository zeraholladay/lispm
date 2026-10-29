#include <stdbool.h>
#include <stdio.h>

#include "bison.h"
#include "flex.h"
#include "lm.h"
#include "prims.h"
#include "types.h"

bool
parser_buf (const char *input, Cell **ast_head, LM *lm)
{
  *ast_head = NIL;
  yy_scan_string (input);

  int status = yyparse (ast_head, lm);

  yylex_destroy ();

  if (status != 0)
    *ast_head = NIL;

  return (status == 0);
}

bool
parser_stream (FILE *instrm, Cell **ast_head, LM *lm)
{
  *ast_head = NIL;
  if (!instrm)
    return false;
  yyset_in (instrm);

  int status = yyparse (ast_head, lm);

  bool success = status == 0 && !ferror (instrm);
  yylex_destroy ();
  if (!success)
    *ast_head = NIL;

  return success;
}
