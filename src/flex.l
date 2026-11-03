%top{
#include "parser_internal.h"
#define YY_DECL int yylex (ParserContext *context)
}

%option noyywrap
%option noinput
%option nounput

%{
#include <errno.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "bison.h"
#include "str_intern.h"

// Location tracking includes matches discarded as comments/whitespace. Do not
// mix this cursor with Flex's yylineno counter: one source of truth per parse.
#define YY_USER_ACTION parser_advance (context, &yylloc, yytext, (size_t)yyleng);
%}

NL       \r?\n
WS       [ \t]+
NUM      -?[0-9]+
FLOAT    -?[0-9]+\.[0-9]+
ID       [a-zA-Z_][a-zA-Z0-9\-_!?]*
BADNUM   -?[0-9]+(\.[0-9]+)?[^ \t\r\n()\[\]';]+

%%

<INITIAL>{
  "'"             {
                    return '\'';
                  }
  "("|"["         {
                    if (!parser_delimiter (context, yylloc, yytext[0]))
                      return ERROR;
                    return '(';
                  }
  ")"|"]"         {
                    if (!parser_delimiter (context, yylloc, yytext[0]))
                      return ERROR;
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
                        parser_report_error (context, yylloc, "Integer literal out of range");
                        return ERROR;
                      }
                    if (end != yytext + yyleng)
                      {
                        parser_report_error (context, yylloc, "Invalid integer literal");
                        return ERROR;
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
                        parser_report_error (context, yylloc, "Floating-point literal out of range");
                        return ERROR;
                      }
                    if (end != yytext + yyleng)
                      {
                        parser_report_error (context, yylloc, "Invalid floating-point literal");
                        return ERROR;
                      }
                    yylval.floating = value;
                    return FLOAT;
                  }
  {BADNUM}        {
                    // A number must end at a delimiter: 1-2, 1e3, 1.5.5 and
                    // 3abc are single invalid tokens. Listed after NUM and
                    // FLOAT so that equal-length valid literals win.
                    parser_report_error (context, yylloc, "Invalid numeric literal");
                    return ERROR;
                  }
  {ID}            {
                    // special types: not symbols
                    if (!strncasecmp ("LAMBDA", yytext, yyleng + 1))
                      return LAMBDA;
                    if (!strncasecmp ("QUOTE", yytext, yyleng + 1))
                      return QUOTE;

                    yylval.symbol.len = yyleng;
                    yylval.symbol.str = str_intern (yytext, yyleng);

                    // special forms but still a symbol; set! has no token
                    // and is an ordinary symbol bound to the native form.
                    if (!strncasecmp ("DEFINE", yytext, yyleng + 1))
                      return DEFINE;
                    if (!strncasecmp ("LET", yytext, yyleng + 1))
                      return LET;
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
                    char message[64];
                    unsigned char byte = (unsigned char)yytext[0];
                    if (isprint (byte))
                      snprintf (message, sizeof (message), "Unexpected character '%c'", byte);
                    else
                      snprintf (message, sizeof (message), "Unexpected byte 0x%02X", byte);
                    parser_report_error (context, yylloc, message);
                    return ERROR;
                  }
  <<EOF>>         {
                    parser_at_eof (context, &yylloc);
                    if (context->delimiter_count
                        && context->delimiters[context->delimiter_count - 1] == '[')
                      parser_report_error (context, yylloc,
                                           "syntax error, unexpected end of file, expecting ']'");
                    return 0;
                  }
}

%%
