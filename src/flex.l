%option noyywrap
%option bison-bridge bison-locations
%option yylineno
%option noinput
%option nounput

%{
#include <stdlib.h>
#include <string.h>

#include "bison.h"
#include "str_intern.h"
%}

NL       \r?\n
WS       [ \t]+
NUM      -?[0-9]+
ID       [a-zA-Z_][a-zA-Z0-9\-_!?]*

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
                    yylval->integer = atoll (yytext);
                    return INTEGER;
                  }
  {ID}            {
                    // special types: not symbols
                    if (!strncasecmp ("LAMBDA", yytext, yyleng + 1))
                      return LAMBDA;
                    if (!strncasecmp ("QUOTE", yytext, yyleng + 1))
                      return QUOTE;

                    yylval->symbol.len = yyleng;
                    yylval->symbol.str = str_intern (yytext, yyleng);

                    // special forms but still a symbol
                    if (!strncasecmp ("DEFINE", yytext, yyleng + 1))
                      return DEFINE;
                    if (!strncasecmp ("LET", yytext, yyleng + 1))
                      return LET;
                    if (!strncasecmp ("SET", yytext, yyleng + 1))
                      return SET;
                    if (!strncasecmp ("IF", yytext, yyleng + 1))
                      return IF;

                    return SYMBOL;
                  }
  [+\-*/><]       {
                    yylval->symbol.str = str_intern (yytext, yyleng);
                    yylval->symbol.len = yyleng;
                    return SYMBOL;
                  }
  {WS}            { /* ignore */ }
  {NL}            { ++yylineno;  }
  ;[^\n]*         { /* ignore */ }
  .               {
                    return ERROR;
                  }
}

%%
