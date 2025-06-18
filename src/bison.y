// clang-format off
%{
#include <stdio.h>

#include "bison.h"
#include "flex.h"
#include "prims.h"

#define yyerror(n, lm, s)                                                     \
  do                                                                          \
    {                                                                         \
      yyerror_handler (lm, s);                                                \
      YYABORT;                                                                \
    }                                                                         \
  while (0)

static inline LType
ltype_from_yyltype(YYLTYPE p)
{
    return (LType){ .first_line   = p.first_line,
                    .first_column = p.first_column,
                    .last_line    = p.last_line,
                    .last_column  = p.last_column };
}

void yyerror_handler (LM *lm, const char *s);
%}

%code requires
{
#include "types.h"
}

%locations
%parse-param {Cell **progn} {LM *lm}

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
      yyerror (progn, lm, "Parse error\n");
    }
  ;

forms
  : /* empty */
    {
      $$ = NIL;
      $$->loc = ltype_from_yyltype (@$);
    }
  | form forms
    {
      $$ = CONS ($1, $2, lm);
      $$->loc = ltype_from_yyltype (@$);
    }
  ;

form
    : '(' LAMBDA param_list forms ')'
      {
        $$ = LIST1 (LAMBDA ($3, $4, lm), lm);
        $$->loc = ltype_from_yyltype (@$);
      }
    | '(' define symbol forms ')'
      {
        $$ = CONS ($2, CONS ($3, $4, lm), lm);
        $$->loc = ltype_from_yyltype (@$);
      }
    | '(' define '(' symbol symbol_list ')' forms ')'
      {
        Cell *body = LIST1 (LAMBDA ($5, $7, lm), lm);
        $$ = CONS ($2, CONS ($4, body, lm), lm);
        $$->loc = ltype_from_yyltype (@$);
      }
    | '(' let forms forms ')'
      {
        $$ = CONS ($2, CONS ($3, $4, lm), lm);
        $$->loc = ltype_from_yyltype (@$);
      }
    | '(' set symbol forms ')'
      {
        $$ = CONS ($2, CONS ($3, $4, lm), lm);
        $$->loc = ltype_from_yyltype (@$);
      }
    | '(' if_ form form ')'
      {
        $$ = CONS ($2, LIST2 ($3, $4, lm), lm);
        $$->loc = ltype_from_yyltype (@$);
      }
    | '(' if_ form form form ')'
      {
        $$ = CONS ($2, CONS ( $3, LIST2 ($4, $5, lm), lm), lm);
        $$->loc = ltype_from_yyltype (@$);
      }
    | symbol
      {
        $$ = $1;
        $$->loc = ltype_from_yyltype (@$);
      }
    | INTEGER
      {
        $$ = INTEGER ($1, lm);
        $$->loc = ltype_from_yyltype (@$);
      }
    | '\'' form
      {
        $$ = LIST2 (QUOTE, $2, lm);
        $$->loc = ltype_from_yyltype (@$);
      }
    | '(' forms ')'
      {
        $$ = $2;
        $$->loc = ltype_from_yyltype (@$);
      }
    | '(' forms '.' form ')'
      {
        $$ = CONS ($2, $4, lm);
        $$->loc = ltype_from_yyltype (@$);
      }
    ;

param_list
  : '(' ')'
    {
      $$ = NIL;
      $$->loc = ltype_from_yyltype (@$);
    }
  | '(' symbol_list ')'
    {
      $$ = $2;
      $$->loc = ltype_from_yyltype (@$);
    }
  ;

symbol_list
  : /* empty */
    {
      $$ = NIL;
      $$->loc = ltype_from_yyltype (@$);
    }
  | symbol symbol_list
    {
      $$ = CONS ($1, $2, lm);
      $$->loc = ltype_from_yyltype (@$);
    }
  ;

symbol
  : SYMBOL
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
      $$->loc = ltype_from_yyltype (@$);
    }
  | QUOTE
    {
      $$ = QUOTE;
      $$->loc = ltype_from_yyltype (@$);
    }
  ;

define
  : DEFINE
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
      $$->loc = ltype_from_yyltype (@$);
    }
  ;

let
  : LET
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
      $$->loc = ltype_from_yyltype (@$);
    }
  ;

set
  : SET
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
      $$->loc = ltype_from_yyltype (@$);
    }
  ;

if_
  : IF
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
      $$->loc = ltype_from_yyltype (@$);
    }
  ;

%%

void
yyerror_handler (LM *lm, const char *s)
{
  (void)lm;
  fprintf (stderr, "Syntax error: line %d: %s\n", yylineno, s);
}
