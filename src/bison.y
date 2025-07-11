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

void yyerror_handler (LM *lm, const char *s);
%}

%code requires
{
#include "types.h"
}

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
    }
  | form forms
    {
      $$ = CONS ($1, $2, lm);
    }
  ;

form
    : '(' LAMBDA param_list forms ')'
      {
        $$ = LIST1 (LAMBDA ($3, $4, lm), lm);
      }
    | '(' define symbol forms ')'
      {
        $$ = CONS ($2, CONS ($3, $4, lm), lm);
      }
    | '(' define '(' symbol symbol_list ')' forms ')'
      {
        Cell *body = LIST1 (LAMBDA ($5, $7, lm), lm);
        $$ = CONS ($2, CONS ($4, body, lm), lm);
      }
    | '(' let forms forms ')'
      {
        $$ = CONS ($2, CONS ($3, $4, lm), lm);
      }
    | '(' set symbol forms ')'
      {
        $$ = CONS ($2, CONS ($3, $4, lm), lm);
      }
    | '(' if_ form form ')'
      {
        $$ = CONS ($2, LIST2 ($3, $4, lm), lm);
      }
    | '(' if_ form form form ')'
      {
        $$ = CONS ($2, CONS ( $3, LIST2 ($4, $5, lm), lm), lm);
      }
    | symbol
      {
        $$ = $1;
      }
    | INTEGER
      {
        $$ = INTEGER ($1, lm);
      }
    | '\'' form
      {
        $$ = LIST2 (QUOTE, $2, lm);
      }
    | '(' forms ')'
      {
        $$ = $2;
      }
    | '(' forms '.' form ')'
      {
        $$ = CONS ($2, $4, lm);  // FIXME
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
    }
  ;

symbol
  : SYMBOL
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
    }
  | QUOTE
    {
      $$ = QUOTE;
    }
  ;

define
  : DEFINE
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
    }
  ;

let
  : LET
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
    }
  ;

set
  : SET
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
    }
  ;

if_
  : IF
    {
      $$ = SYMBOL ($1.str, $1.len, lm);
    }
  ;

%%

void
yyerror_handler (LM *lm, const char *s)
{
  (void)lm;
  fprintf (stderr, "Syntax error: line %d: %s\n", yylineno, s);
}
