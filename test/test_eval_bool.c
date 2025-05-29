#include <check.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "parser.h"
#include "repl.h"
#include "types.h"

extern void lispm_init (Context *ctx);
extern void lispm_destroy (Context *ctx);

static Cell *progn = NULL;
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

static Cell *
run_eval_progn (const char *input)
{
  ck_assert (parser_buf (input, &progn, &ctx));
  return eval_progn (progn, &ctx);
}

START_TEST (test_eq)
{
  Cell *eval_result = NULL;
  char *test_program;

  // True statements
  eval_result = run_eval_progn ("(eq T T)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(eq NIL NIL)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(eq 0 0)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(eq 42 42)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(eq '() '())");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(eq 'foo 'foo)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  test_program = "(set 'foo (lambda () ()))"
                 "(set 'bar foo)"
                 "(eq foo bar)";
  eval_result = run_eval_progn (test_program);
  ck_assert_str_eq (eval_result->symbol.str, "T");

  test_program = "(set 'foo '(1 2 3 4))"
                 "(set 'bar foo)"
                 "(eq foo bar)";
  eval_result = run_eval_progn (test_program);
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(eq (string 'foo) (string 'foo))");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(eq rest rest)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  // False statements
  eval_result = run_eval_progn ("(eq T NIL)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(eq NIL T)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(eq 0 1)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(eq -42 42)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(eq '() '(1))");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(eq 'foo 'bar)");
  ck_assert (IS_NIL (eval_result));

  test_program = "(set 'foo (lambda () ()))"
                 "(set 'bar (lambda () ()))"
                 "(eq foo bar)";
  eval_result = run_eval_progn (test_program);
  ck_assert (IS_NIL (eval_result));

  test_program = "(set 'foo '(1 2 3 4))"
                 "(set 'bar '(1 2 3 4))"
                 "(eq foo bar)";
  eval_result = run_eval_progn (test_program);
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(eq (string 'foo) (string 'bar))");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(eq first rest)");
  ck_assert (IS_NIL (eval_result));
}
END_TEST

START_TEST (test_not)
{
  Cell *eval_result = NULL;

  eval_result = run_eval_progn ("(not nil)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(not T)");
  ck_assert (IS_NIL (eval_result));
}
END_TEST

START_TEST (test_and)
{
  Cell *eval_result = NULL;

  eval_result = run_eval_progn ("(and nil T)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(and T nil)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(and T T)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(and)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(and T T 'foobar)");
  ck_assert_str_eq (eval_result->symbol.str, "foobar");
}
END_TEST

START_TEST (test_or)
{
  Cell *eval_result = NULL;

  eval_result = run_eval_progn ("(or nil nil)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(or T nil)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(or nil T)");
  ck_assert_str_eq (eval_result->symbol.str, "T");

  eval_result = run_eval_progn ("(or)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(or 'foobar T T)");
  ck_assert_str_eq (eval_result->symbol.str, "foobar");
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
