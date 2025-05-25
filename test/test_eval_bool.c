#include <check.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "parser.h"
#include "repl.h"
#include "types.h"

extern FILE *yyin;
extern int yyparse (Context *ctx);
extern void yylex_destroy (void);

extern void lispm_init (Context *ctx);
extern void lispm_destroy (Context *ctx);

static Context ctx = {};

jmp_buf eval_error_jmp;

static void
setup (void)
{
  lispm_init (&ctx);
}

static void
teardown (void)
{
  lispm_destroy (&ctx);
}

static Node *
run_eval_program (const char *input)
{
  yyin = fmemopen ((void *)input, strlen (input), "r");

  int parse_status = yyparse (&ctx);
  ck_assert_int_eq (parse_status, 0);

  Node *program = CTX_PARSE_ROOT (&ctx);
  Node *eval_result = eval_program (program, &ctx);

  yylex_destroy ();
  fclose (yyin);

  return eval_result;
}

START_TEST (test_eq)
{
  Node *eval_result = NULL;
  char *test_program;

  // True statements
  eval_result = run_eval_program ("(eq T T)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(eq NIL NIL)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(eq 0 0)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(eq 42 42)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(eq '() '())");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(eq 'foo 'foo)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  test_program = "(set 'foo (lambda () ()))"
                 "(set 'bar foo)"
                 "(eq foo bar)";
  eval_result = run_eval_program (test_program);
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  test_program = "(set 'foo '(1 2 3 4))"
                 "(set 'bar foo)"
                 "(eq foo bar)";
  eval_result = run_eval_program (test_program);
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(eq (str 'foo) (str 'foo))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(eq rest rest)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  // False statements
  eval_result = run_eval_program ("(eq T NIL)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(eq NIL T)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(eq 0 1)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(eq -42 42)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(eq '() '(1))");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(eq 'foo 'bar)");
  ck_assert (IS_NIL (eval_result));

  test_program = "(set 'foo (lambda () ()))"
                 "(set 'bar (lambda () ()))"
                 "(eq foo bar)";
  eval_result = run_eval_program (test_program);
  ck_assert (IS_NIL (eval_result));

  test_program = "(set 'foo '(1 2 3 4))"
                 "(set 'bar '(1 2 3 4))"
                 "(eq foo bar)";
  eval_result = run_eval_program (test_program);
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(eq (str 'foo) (str 'bar))");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(eq first rest)");
  ck_assert (IS_NIL (eval_result));
}
END_TEST

START_TEST (test_not)
{
  Node *eval_result = NULL;

  eval_result = run_eval_program ("(not nil)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(not T)");
  ck_assert (IS_NIL (eval_result));
}
END_TEST

START_TEST (test_and)
{
  Node *eval_result = NULL;

  eval_result = run_eval_program ("(and nil T)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(and T nil)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(and T T)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(and)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(and T T 'foobar)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "foobar");
}
END_TEST

START_TEST (test_or)
{
  Node *eval_result = NULL;

  eval_result = run_eval_program ("(or nil nil)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(or T nil)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(or nil T)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("(or)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(or 'foobar T T)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "foobar");
}
END_TEST

Suite *
eval_bool_suite (void)
{
  Suite *s = suite_create ("Eval Boolean");

  TCase *tc_core = tcase_create ("Core");
  tcase_add_checked_fixture (tc_core, setup, teardown);

  tcase_add_test (tc_core, test_eq);
  tcase_add_test (tc_core, test_not);
  tcase_add_test (tc_core, test_and);
  tcase_add_test (tc_core, test_or);

  suite_add_tcase (s, tc_core);
  return s;
}
