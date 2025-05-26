// clang-format off
%{
#include <stdio.h>

#include "eval.h"
#include "parser.h"
#include "types.h"

#define yyerror(ctx, s)                                                       \
  do                                                                          \
    {                                                                         \
      yyerror_handler (ctx, s);                                               \
      YYABORT;                                                                \
    }                                                                         \
  while (0)

int yylex (Context * ctx);
void yyerror_handler (Context * ctx, const char *s);

extern int yylineno;
%}

%code requires
{
#include "types.h"

void reset_parse_context(Context *ctx);
}

%lex-param
{
Context *ctx
}

%parse-param
{
Context *ctx
}

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
      CTX_PARSE_ROOT (ctx) = $1;
      YYACCEPT;
    }
  | forms error
    {
      CTX_PARSE_ROOT (ctx) = NIL;
      yyerror (ctx, "Parse error\n");
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
        $$ = LIST1 (cons_lambda (&CTX_POOL (ctx), $3, $4, NULL), ctx);
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
        $$ = cons_integer (&CTX_POOL (ctx), $1);
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
      $$ = cons_symbol (&CTX_POOL (ctx), $1.str, $1.len);
    }
  | QUOTE
    {
      $$ = KEYWORD (QUOTE);
    }
  ;

if_
  : IF
    {
      $$ = cons_symbol (&CTX_POOL (ctx), $1.str, $1.len);
    }
  ;

%%
void
reset_parse_context (Context *ctx)
{
  /* assumes pool has already been allocated. */
  CTX_PARSE_ROOT (ctx) = NIL;
}

void
yyerror_handler (Context *ctx, const char *s)
{
  fprintf (stderr, "Syntax error: line %d: %s\n", yylineno, s);
  reset_parse_context (ctx);
}

// clang-format off
