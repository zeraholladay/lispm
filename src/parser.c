#include <stdio.h>

#include "parser.h"

#if YYDEBUG
extern int yydebug;
#endif

#ifndef INPUT_BUF_SIZ
#define INPUT_BUF_SIZ 8192
#endif

extern FILE *yyin;
extern int yyparse (Context *ctx);
extern void yylex_destroy (void);

bool
parser_rl (const char *str, Node *parse_head, Context *ctx)
{
  char full_input[INPUT_BUF_SIZ];
  int len = rl_readline (full_input, sizeof (full_input));

  if (len < 0)
    return false;

  yyin = fmemopen ((void *)full_input, len, "r");

  reset_parse_context (ctx);
  int parse_status = yyparse (ctx);

  yylex_destroy ();
  fclose (yyin);

  if (parse_status)
    return false;

  return true;
}

bool
parser_stream (FILE *restrict in, Node *parse_head, Context *ctx)
{
  yyin = in;

  int parse_status = yyparse (&ctx);

  fclose (yyin);

  if (parse_status)
    return false;

  return true;
}
