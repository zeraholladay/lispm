#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lisp_headers.h"
#include "lisp_mach.h"
#include "lisp_types.h"
#include "parser.h"
#include "repl.h"

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

START_TEST (test_gt)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(gt 10 5 2)");
  ck_assert_str_eq (eval_res->symbol.str, "T");

  eval_res = run_eval_progn ("(gt 5)");
  ck_assert_str_eq (eval_res->symbol.str, "T");

  eval_res = run_eval_progn ("(gt 2 3)");
  ck_assert (NILP (eval_res));

  eval_res = run_eval_progn ("(gt 3 3)");
  ck_assert (NILP (eval_res));
}
END_TEST

START_TEST (test_lt)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(lt 1 2 3)");
  ck_assert_str_eq (eval_res->symbol.str, "T");

  eval_res = run_eval_progn ("(lt 5)");
  ck_assert_str_eq (eval_res->symbol.str, "T");

  eval_res = run_eval_progn ("(lt 3 2)");
  ck_assert (NILP (eval_res));

  eval_res = run_eval_progn ("(lt 4 4)");
  ck_assert (NILP (eval_res));
}
END_TEST

START_TEST (test_add)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(add 1 2 3)");
  ck_assert_int_eq (eval_res->integer, 6);

  eval_res = run_eval_progn ("(add 5)");
  ck_assert_int_eq (eval_res->integer, 5);

  eval_res = run_eval_progn ("(add 0 -3 7)");
  ck_assert_int_eq (eval_res->integer, 4);
}
END_TEST

START_TEST (test_sub)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(sub 10 2 3)");
  ck_assert_int_eq (eval_res->integer, 5);

  eval_res = run_eval_progn ("(sub 5)");
  ck_assert_int_eq (eval_res->integer, 5);

  eval_res = run_eval_progn ("(sub 2 5)");
  ck_assert_int_eq (eval_res->integer, -3);
}
END_TEST

START_TEST (test_mul)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(mul 2 3 4)");
  ck_assert_int_eq (eval_res->integer, 24);

  eval_res = run_eval_progn ("(mul 7)");
  ck_assert_int_eq (eval_res->integer, 7);

  eval_res = run_eval_progn ("(mul 0 10 5)");
  ck_assert_int_eq (eval_res->integer, 0);
}
END_TEST

START_TEST (test_div)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(div 100 2 5)");
  ck_assert_int_eq (eval_res->integer, 10);

  eval_res = run_eval_progn ("(div 42)");
  ck_assert_int_eq (eval_res->integer, 42);

  eval_res = run_eval_progn ("(div 10 0)");
  ck_assert (IS_INST (eval_res, NIL));
}
END_TEST

Suite *
eval_math_suite (void)
{
  Suite *s = suite_create ("Eval Math");

  TCase *tc_core = tcase_create ("Core");
  tcase_add_checked_fixture (tc_core, setup, teardown);

  tcase_add_test (tc_core, test_gt);
  tcase_add_test (tc_core, test_lt);
  tcase_add_test (tc_core, test_add);
  tcase_add_test (tc_core, test_sub);
  tcase_add_test (tc_core, test_mul);
  tcase_add_test (tc_core, test_div);

  suite_add_tcase (s, tc_core);
  return s;
}
