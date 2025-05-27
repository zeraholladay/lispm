// clang-format off
%{
#include <stdio.h>

#include "ctx.h"
#include "parser.h"
#include "types.h"

int yylex (Ctx * ctx);
extern int yylineno;
%}

%parse-param {Node *progn} {Ctx *ctx}
%lex-param {Ctx *ctx}

%union
{
  Integer integer;
  Node *node;
  struct {
    const char *str;
    size_t len;
  } symbol;
}

%token ERROR
%token <integer> INTEGER
%token <symbol>  IF SYMBOL
%token LAMBDA QUOTE

%type <node>
  program
  forms
  form
  if_
  symbol
  symbol_list
  param_list
%%

program
  : forms
    {
      progn = $1;
      YYACCEPT;
    }
  | forms error
    {
      YYABORT;
    }
  ;

forms
  : /* empty */
    {
      $$ = NIL;
    }
  | form forms
    {
      $$ = CONS ($1, $2, ctx);
    }
  ;

form
    : '(' LAMBDA param_list forms ')'
      {
        $$ = LIST1 (cons_lambda ($3, $4, NULL, ctx), ctx);
      }
    | '(' if_ form form ')'
      {
        $$ = CONS ($2, LIST2 ($3, $4, ctx), ctx);
      }
    | '(' if_ form form form ')'
      {
        $$ = CONS ($2, CONS ( $3, LIST2 ($4, $5, ctx), ctx), ctx);
      }
    | symbol
      {
        $$ = $1;
      }
    | INTEGER
      {
        $$ = cons_integer ($1, ctx);
      }
    | '\'' form
      {
        $$ = LIST2 (KEYWORD (QUOTE), $2, ctx);
      }
    | '(' forms ')'
      {
        $$ = $2;
      }
    | '(' forms '.' form ')'
      {
        $$ = CONS ($2, $4, ctx);  // FIXME
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
      $$ = CONS ($1, $2, ctx);
    }
  ;

symbol
  : SYMBOL
    {
      $$ = cons_symbol ($1.str, $1.len, ctx);
    }
  | QUOTE
    {
      $$ = KEYWORD (QUOTE);
    }
  ;

if_
  : IF
    {
      $$ = cons_symbol ($1.str, $1.len, ctx);
    }
  ;

%%
