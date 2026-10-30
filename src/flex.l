%option noyywrap
%option yylineno
%option noinput
%option nounput

%{
#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "bison.h"
#include "str_intern.h"
%}

NL       \r?\n
WS       [ \t]+
NUM      -?[0-9]+
FLOAT    -?[0-9]+\.[0-9]+
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
                    char *end;
                    // errno is sticky: a previous token's range error must
                    // not cause a valid literal to fail conversion.
                    errno = 0;
                    Integer value = strtoll (yytext, &end, 10);
                    if (errno == ERANGE)
                      {
                        yylval.error = "Integer literal out of range";
                        return NUMERIC_ERROR;
                      }
                    if (end != yytext + yyleng)
                      {
                        yylval.error = "Invalid integer literal";
                        return NUMERIC_ERROR;
                      }
                    yylval.integer = value;
                    return INTEGER;
                  }
  {FLOAT}         {
                    char *end;
                    errno = 0;
                    Float value = strtod (yytext, &end);
                    // Reject overflow and reported underflow, including a
                    // nonzero input rounded down to zero by strtod.
                    if (errno == ERANGE || !isfinite (value))
                      {
                        yylval.error = "Floating-point literal out of range";
                        return NUMERIC_ERROR;
                      }
                    if (end != yytext + yyleng)
                      {
                        yylval.error = "Invalid floating-point literal";
                        return NUMERIC_ERROR;
                      }
                    yylval.floating = value;
                    return FLOAT;
                  }
  {ID}            {
                    // special types: not symbols
                    if (!strncasecmp ("LAMBDA", yytext, yyleng + 1))
                      return LAMBDA;
                    if (!strncasecmp ("QUOTE", yytext, yyleng + 1))
                      return QUOTE;

                    yylval.symbol.len = yyleng;
                    yylval.symbol.str = str_intern (yytext, yyleng);

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
  "<="|">="|[+\-*/><=] {
                    yylval.symbol.str = str_intern (yytext, yyleng);
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
