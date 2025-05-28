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

static Node *progn = NULL;
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
run_eval_progn (const char *input)
{
  ck_assert (parser_buf (input, &progn, &ctx));
  return eval_progn (progn, &ctx);
}

START_TEST (test_gt)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(gt 10 5 2)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_progn ("(gt 5)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_progn ("(gt 2 3)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(gt 3 3)");
  ck_assert (IS_NIL (eval_result));
}
END_TEST

START_TEST (test_lt)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(lt 1 2 3)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_progn ("(lt 5)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_progn ("(lt 3 2)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(lt 4 4)");
  ck_assert (IS_NIL (eval_result));
}
END_TEST

START_TEST (test_add)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(add 1 2 3)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 6);

  eval_result = run_eval_progn ("(add 5)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 5);

  eval_result = run_eval_progn ("(add 0 -3 7)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 4);
}
END_TEST

START_TEST (test_sub)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(sub 10 2 3)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 5);

  eval_result = run_eval_progn ("(sub 5)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 5);

  eval_result = run_eval_progn ("(sub 2 5)");
  ck_assert_int_eq (GET_INTEGER (eval_result), -3);
}
END_TEST

START_TEST (test_mul)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(mul 2 3 4)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 24);

  eval_result = run_eval_progn ("(mul 7)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 7);

  eval_result = run_eval_progn ("(mul 0 10 5)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 0);
}
END_TEST

START_TEST (test_div)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(div 100 2 5)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 10);

  eval_result = run_eval_progn ("(div 42)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 42);

  if (setjmp (eval_error_jmp) == 0)
    {

      eval_result = run_eval_progn ("(div 10 0)");
      ck_assert (0);
    }
  else
    {
      ck_assert (1);
    }
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
