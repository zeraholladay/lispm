// clang-format off
%require "3.6"
%locations
%define api.location.type {SourceLocation}
%define parse.error detailed
%define parse.lac full

%{
#include "bison.h"
#include "keywords.h"
#include "prims.h"

void yyerror (Cell **progn, LM *lm, ParserContext *context, const char *message);
static bool valid_formals (Cell *params);
%}

%code requires
{
#include "types.h"
#include "parser_internal.h"
}

%parse-param {Cell **progn} {LM *lm} {ParserContext *context}
%lex-param {ParserContext *context}

%union
{
  Integer integer;
  Float floating;
  Cell *cell;
  struct {
    const char *str;
    size_t len;
  } symbol;
}

%token ERROR "invalid token" LAMBDA "lambda" QUOTE "quote"
%token <integer> INTEGER "integer"
%token <floating> FLOAT "float"
%token <symbol> IF "if" DEFINE "define" LET "let" SET "set" SYMBOL "identifier"

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
          {
            parser_report_error (context, @3, "Lambda parameters must be distinct, non-reserved symbols");
            YYABORT;
          }
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
          {
            parser_report_error (context, @5, "Function parameters must be distinct, non-reserved symbols");
            YYABORT;
          }
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
yyerror (Cell **progn, LM *lm, ParserContext *context, const char *message)
{
  (void)progn;
  (void)lm;
  parser_report_error (context, yylloc, message);
}
