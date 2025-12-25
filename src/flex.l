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

#define YY_USER_ACTION                            \
    yylloc->first_line = yylloc->last_line;       \
    yylloc->first_column = yylloc->last_column;   \
    for (int i = 0; yytext[i] != '\0'; i++) {     \
        if (yytext[i] == '\n') {                  \
            yylloc->last_line++;                  \
            yylloc->last_column = 1;              \
        } else {                                  \
            yylloc->last_column++;                \
        }                                         \
    }
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
                    yylval->integer = atoll (yytext);
                    return INTEGER;
                  }
  {FLOAT}         {
                    yylval._float = atof (yytext);
                    return FLOAT;
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
  {NL}            { /* handled by YY_USER_ACTION */ }
  ;[^\n]*         { /* ignore */ }
  .               {
                    return ERROR;
                  }
}

%%
