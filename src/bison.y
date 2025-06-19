// clang-format off
%{
#include <stdio.h>

#include "bison.h"
#include "flex.h"
#include "lm.h"
#include "prims.h"

#define yyerror(progn, parser_ptr, lm, s)                                       \
  do                                                                          \
    {                                                                         \
      yyerror_handler (s);                                                    \
      YYABORT;                                                                \
    }                                                                         \
  while (0)

void yyerror_handler (const char *s);

static inline LType
ltype_from_yyltype(YYLTYPE p, void **parser_ptr)
{
  return (LType){ .parser_ptr   = parser_ptr,
                  .first_line   = p.first_line,
                  .first_column = p.first_column,
                  .last_line    = p.last_line,
                  .last_column  = p.last_column };
}
%}

%code requires
{
#include "types.h"
}

%locations
%parse-param {Cell **progn} {void **parser_ptr} {LM *lm}

%union
{
  Integer integer;
  Cell *cell;
  struct {
    const char *str;
    size_t len;
  } symbol;
}

%token ERROR LAMBDA QUOTE
%token <integer> INTEGER
%token <symbol>  IF DEFINE LET SET SYMBOL

%type <cell>
  program
  forms
  form
  if_
  define
  let
  set
  symbol
  symbol_list
  param_list
%%

program
  : forms
    {
      *progn = $1;
      YYACCEPT;
    }
  | forms error
    {
      *progn = NIL;
      yyerror (progn, lm, parser_ptr, "Parse error\n");
    }
  ;

forms
  : /* empty */
    {
      $$ = NIL;
    }
  | form forms
    {
      $$ = CONS ($1, $2, lm);
      $$->loc = ltype_from_yyltype (@$, parser_ptr);
    }
  ;

form
    : '(' LAMBDA param_list forms ')'
      {
        $$ = LIST1 (LAMBDA ($3, $4, lm), lm);
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | '(' define symbol forms ')'
      {
        $$ = CONS ($2, CONS ($3, $4, lm), lm);
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | '(' define '(' symbol symbol_list ')' forms ')'
      {
        Cell *body = LIST1 (LAMBDA ($5, $7, lm), lm);
        $$ = CONS ($2, CONS ($4, body, lm), lm);
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | '(' let forms forms ')'
      {
        $$ = CONS ($2, CONS ($3, $4, lm), lm);
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | '(' set symbol forms ')'
      {
        $$ = CONS ($2, CONS ($3, $4, lm), lm);
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | '(' if_ form form ')'
      {
        $$ = CONS ($2, LIST2 ($3, $4, lm), lm);
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | '(' if_ form form form ')'
      {
        $$ = CONS ($2, CONS ( $3, LIST2 ($4, $5, lm), lm), lm);
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | symbol
      {
        $$ = $1;
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | INTEGER
      {
        $$ = INTEGER ($1, lm);
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | '\'' form
      {
        $$ = LIST2 (QUOTE, $2, lm);
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | '(' forms ')'
      {
        $$ = $2;
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    | '(' forms '.' form ')'
      {
        $$ = CONS ($2, $4, lm);
        $$->loc = ltype_from_yyltype (@$, parser_ptr);
      }
    ;

param_list
  : '(' ')'
    {
      $$ = NIL;
    }
  | '(' symbol_list ')'
    {
      $$ = $2;
      $$->loc = ltype_from_yyltype (@$, parser_ptr);
    }
  ;

symbol_list
  : /* empty */
    {
      $$ = NIL;
    }
  | symbol symbol_list
    {
      $$ = CONS ($1, $2, lm);
      $$->loc = ltype_from_yyltype (@$, parser_ptr);
    }
  ;

symbol
  : SYMBOL
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
      $$->loc = ltype_from_yyltype (@$, parser_ptr);
    }
  | QUOTE
    {
      $$ = QUOTE;
      $$->loc = ltype_from_yyltype (@$, parser_ptr);
    }
  ;

define
  : DEFINE
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
      $$->loc = ltype_from_yyltype (@$, parser_ptr);
    }
  ;

let
  : LET
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
      $$->loc = ltype_from_yyltype (@$, parser_ptr);
    }
  ;

set
  : SET
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
      $$->loc = ltype_from_yyltype (@$, parser_ptr);
    }
  ;

if_
  : IF
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
      $$->loc = ltype_from_yyltype (@$, parser_ptr);
    }
  ;

%%

void
yyerror_handler (const char *s)
{
  fprintf (stderr, "Syntax error: line %d: %s\n", yylineno, s);
}
