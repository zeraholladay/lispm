%option noyywrap
%option yylineno
%option noinput
%option nounput

%{
#include <stdlib.h>

#include "debug.h"
#include "eval_ctx.h"
#include "parser.h"
#include "safe_str.h"
#include "sym_save.h"

#define YY_DECL int yylex(Context *ctx)
%}

NL       \r?\n
WS       [ \t]+
NUM      -?[0-9]+
ID       [a-zA-Z_][a-zA-Z0-9_]*

%%

<INITIAL>{
  "'"             {
                    return '\'';
                  }
  "("             {
                    return '(';
                  }
  ")"             {
                    return ')';
                  }
  "."             {
                    return '.';
                  }
  {NUM}           {
                    yylval.integer = atoll (yytext);
                    return INTEGER;
                  }

  {ID}            {
                    if (!strncasecmp ("QUOTE", yytext, yyleng + 1))
                      {
                        return QUOTE;
                      }
                    if (!strncasecmp ("LAMBDA", yytext, yyleng + 1))
                      {
                        return LAMBDA;
                      }
                    else if (!strncasecmp ("IF", yytext, yyleng + 1))
                      {
                        return IF;
                      }
                    else
                      {
                        yylval.symbol.len = yyleng;
                        yylval.symbol.str = sym_save (&CTX_SYMTAB(ctx), yytext, yyleng);
                        return SYMBOL;
                      }
                  }

  [+\-*/><]       {
                    yylval.symbol.str = sym_save (&CTX_SYMTAB(ctx), yytext, yyleng);
                    yylval.symbol.len = yyleng;
                    return SYMBOL;
                  }
  {WS}            { /* ignore */ }
  {NL}            { /* ignore */ }
  ;[^\n]*         { /* ignore */ }
  .               {
                    return ERROR;
                  }
}

%%
