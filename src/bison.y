// clang-format off
%{
#include <stdio.h>

#include "bison.h"
#include "flex.h"
#include "keywords.h"
#include "prims.h"

#define yyerror(n, lm, s)                                                     \
  do                                                                          \
    {                                                                         \
      yyerror_handler (lm, s);                                                \
      YYABORT;                                                                \
    }                                                                         \
  while (0)

void yyerror_handler (LM *lm, const char *s);
static bool valid_formals (Cell *params);
%}

%code requires
{
#include "types.h"
}

%parse-param {Cell **progn} {LM *lm}

%union
{
  Integer integer;
  Float _float;
  const char *error;
  Cell *cell;
  struct {
    const char *str;
    size_t len;
  } symbol;
}

%token ERROR LAMBDA QUOTE
%token <integer> INTEGER
%token <_float> FLOAT
%token <error> NUMERIC_ERROR
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
  formal_tail
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
        if (!valid_formals ($3))
          yyerror (progn, lm, "Lambda parameters must be distinct, non-reserved symbols");
        $$ = LIST1 (LAMBDA ($3, $4, lm), lm);
      }
    | '(' define symbol forms ')'
      {
        $$ = CONS ($2, CONS ($3, $4, lm), lm);
      }
    | '(' define '(' symbol formal_tail ')' forms ')'
      {
        if (!valid_formals ($5))
          yyerror (progn, lm, "Function parameters must be distinct, non-reserved symbols");
        Cell *body = LIST1 (LAMBDA ($5, $7, lm), lm);
        $$ = CONS ($2, CONS ($4, body, lm), lm);
      }
    | '(' let form forms ')'
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
    | FLOAT
      {
        $$ = FLOAT ($1, lm);
      }
    | NUMERIC_ERROR
      {
        // Abort parsing before an out-of-range value can enter the AST.
        yyerror (progn, lm, $1);
      }
    | '\'' form
      {
        $$ = LIST2 (QUOTE, $2, lm);
      }
    | '(' forms ')'
      {
        $$ = $2;
      }
    | '(' form forms '.' form ')'
      {
        Cell *head = CONS ($2, $3, lm);
        Cell *tail = head;
        while (!NILP (CDR (tail)))
          tail = CDR (tail);
        RPLACD (tail, $5);
        $$ = head;
      }
    ;

param_list
  : symbol
    {
      // A bare symbol receives every argument, including an empty list.
      $$ = $1;
    }
  | '(' ')'
    {
      $$ = NIL;
    }
  | '(' symbol formal_tail ')'
    {
      // Requiring a first symbol here rejects the malformed (. rest) form.
      $$ = CONS ($2, $3, lm);
    }
  ;

formal_tail
  : /* empty */
    {
      $$ = NIL;
    }
  | symbol formal_tail
    {
      $$ = CONS ($1, $2, lm);
    }
  | '.' symbol
    {
      // A symbol tail represents the rest binding, not a required parameter.
      $$ = $2;
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

static bool
valid_formals (Cell *params)
{
  // Validate at definition time, even if the function is never called. The
  // grammar enforces the shape; this checks both fixed and rest names against
  // reserved words and against each other, without rejecting lexical shadowing.
  Dict *seen = dict_create (NULL, 0);
  bool valid = true;
  for (Cell *p = params; !NILP (p); p = CONSP (p) ? CDR (p) : NIL)
    {
      Cell *name = CONSP (p) ? CAR (p) : p;
      if (!IS_INST (name, SYMBOL)
          || keyword_lookup (name->symbol.str, name->symbol.len)
          || dict_has_key (seen, name->symbol.str))
        {
          valid = false;
          break;
        }
      dict_insert (seen, name->symbol.str, name);
    }
  dict_destroy (seen);
  return valid;
}

void
yyerror_handler (LM *lm, const char *s)
{
  (void)lm;
  fprintf (stderr, "Syntax error: line %d: %s\n", yylineno, s);
}
