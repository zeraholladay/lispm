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
  Float floating;
  const char *error;
  Cell *cell;
  struct {
    const char *str;
    size_t len;
  } symbol;
}

%token ERROR LAMBDA QUOTE
%token <integer> INTEGER
%token <floating> FLOAT
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
      $$ = cell_make_pair (lm, $1, $2);
    }
  ;

form
    : '(' LAMBDA param_list forms ')'
      {
        if (!valid_formals ($3))
          yyerror (progn, lm, "Lambda parameters must be distinct, non-reserved symbols");
        $$ = cell_list1 (lm, cell_make_lambda (lm, $3, $4));
      }
    | '(' define symbol forms ')'
      {
        $$ = cell_make_pair (lm, $2, cell_make_pair (lm, $3, $4));
      }
    | '(' define '(' symbol formal_tail ')' forms ')'
      {
        // Named-definition shorthand lowers to an ordinary binding of a
        // lambda AST; the VM needs no separate function-definition operation.
        if (!valid_formals ($5))
          yyerror (progn, lm, "Function parameters must be distinct, non-reserved symbols");
        Cell *body = cell_list1 (lm, cell_make_lambda (lm, $5, $7));
        $$ = cell_make_pair (lm, $2, cell_make_pair (lm, $4, body));
      }
    | '(' let form forms ')'
      {
        $$ = cell_make_pair (lm, $2, cell_make_pair (lm, $3, $4));
      }
    | '(' set symbol forms ')'
      {
        $$ = cell_make_pair (lm, $2, cell_make_pair (lm, $3, $4));
      }
    | '(' if_ form form ')'
      {
        $$ = cell_make_pair (lm, $2, cell_list2 (lm, $3, $4));
      }
    | '(' if_ form form form ')'
      {
        $$ = cell_make_pair (lm, $2, cell_make_pair (lm, $3, cell_list2 (lm, $4, $5)));
      }
    | symbol
      {
        $$ = $1;
      }
    | INTEGER
      {
        $$ = cell_make_integer (lm, $1);
      }
    | FLOAT
      {
        $$ = cell_make_float (lm, $1);
      }
    | NUMERIC_ERROR
      {
        // Abort parsing before an out-of-range value can enter the AST.
        yyerror (progn, lm, $1);
      }
    | '\'' form
      {
        $$ = cell_list2 (lm, QUOTE, $2);
      }
    | '(' forms ')'
      {
        $$ = $2;
      }
    | '(' form forms '.' form ')'
      {
        Cell *head = cell_make_pair (lm, $2, $3);
        Cell *tail = head;
        while (!cell_is_nil (CDR (tail)))
          tail = CDR (tail);
        cell_set_cdr (tail, $5);
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
      $$ = cell_make_pair (lm, $2, $3);
    }
  ;

formal_tail
  : /* empty */
    {
      $$ = NIL;
    }
  | symbol formal_tail
    {
      $$ = cell_make_pair (lm, $1, $2);
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
      $$ = cell_make_symbol (lm, $1.str, $1.len);
    }
  | QUOTE
    {
      $$ = QUOTE;
    }
  ;

define
  : DEFINE
    {
      $$ = cell_make_symbol (lm, $1.str, $1.len);
    }
  ;

let
  : LET
    {
      $$ = cell_make_symbol (lm, $1.str, $1.len);
    }
  ;

set
  : SET
    {
      $$ = cell_make_symbol (lm, $1.str, $1.len);
    }
  ;

if_
  : IF
    {
      $$ = cell_make_symbol (lm, $1.str, $1.len);
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
  for (Cell *p = params; !cell_is_nil (p); p = cell_is_pair (p) ? CDR (p) : NIL)
    {
      Cell *name = cell_is_pair (p) ? CAR (p) : p;
      if (!cell_has_type (name, CELL_TYPE_SYMBOL)
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
