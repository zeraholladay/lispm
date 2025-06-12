#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lm.h"
#include "parser.h"
#include "prims.h"
#include "repl.h"
#include "types.h"

static Cell *progn = NULL;
static LM *lm = NULL;

static void
setup (void)
{
  lm = lm_create ();
}

static void
teardown (void)
{
  lm_destroy (lm);
}

static Cell *
run_eval_progn (const char *input)
{
  ck_assert (parser_buf (input, &progn, lm));
  return lm_progn (lm, progn);
}

START_TEST (test_eq)
{
  Cell *eval_result = NULL;
  char *test_program;

  // True statements
  eval_result = run_eval_progn ("(eq T T)");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(eq NIL NIL)");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(eq 0 0)");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(eq 42 42)");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(eq '() '())");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(eq 'foo 'foo)");
  ck_assert_ptr_eq (eval_result, T);

  test_program = "(set 'foo (lambda () ()))"
                 "(set 'bar foo)"
                 "(eq foo bar)";
  eval_result = run_eval_progn (test_program);
  ck_assert_ptr_eq (eval_result, T);

  test_program = "(set 'foo '(1 2 3 4))"
                 "(set 'bar foo)"
                 "(eq foo bar)";
  eval_result = run_eval_progn (test_program);
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(eq (string 'foo) (string 'foo))");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(eq rest rest)");
  ck_assert_ptr_eq (eval_result, T);

  // False statements
  eval_result = run_eval_progn ("(eq T NIL)");
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(eq NIL T)");
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(eq 0 1)");
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(eq -42 42)");
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(eq '() '(1))");
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(eq 'foo 'bar)");
  ck_assert (NILP (eval_result));

  test_program = "(set 'foo (lambda () ()))"
                 "(set 'bar (lambda () ()))"
                 "(eq foo bar)";
  eval_result = run_eval_progn (test_program);
  ck_assert (NILP (eval_result));

  test_program = "(set 'foo '(1 2 3 4))"
                 "(set 'bar '(1 2 3 4))"
                 "(eq foo bar)";
  eval_result = run_eval_progn (test_program);
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(eq (string 'foo) (string 'bar))");
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(eq first rest)");
  ck_assert (NILP (eval_result));
}
END_TEST

START_TEST (test_not)
{
  Cell *eval_result = NULL;

  eval_result = run_eval_progn ("(not nil)");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(not T)");
  ck_assert (NILP (eval_result));
}
END_TEST

START_TEST (test_and)
{
  Cell *eval_result = NULL;

  eval_result = run_eval_progn ("(and nil T)");
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(and T nil)");
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(and T T)");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(and)");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(and T T 'foobar)");
  ck_assert_str_eq (eval_result->symbol.str, "foobar");
}
END_TEST

START_TEST (test_or)
{
  Cell *eval_result = NULL;

  eval_result = run_eval_progn ("(or nil nil)");
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(or T nil)");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(or nil T)");
  ck_assert_ptr_eq (eval_result, T);

  eval_result = run_eval_progn ("(or)");
  ck_assert (NILP (eval_result));

  eval_result = run_eval_progn ("(or 'foobar T T)");
  ck_assert_str_eq (eval_result->symbol.str, "foobar");
}
END_TEST

Suite *
eval_bool_suite (void)
{
  Suite *s = suite_create ("Eval Boolean");

  TCase *tc = tcase_create ("Core");
  tcase_add_checked_fixture (tc, setup, teardown);

  tcase_add_test (tc, test_eq);
  tcase_add_test (tc, test_not);
  tcase_add_test (tc, test_and);
  tcase_add_test (tc, test_or);

  suite_add_tcase (s, tc);
  return s;
}
