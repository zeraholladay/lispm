#include <check.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lm.h"
#include "parser.h"
#include "prims.h"
#include "repl.h"
#include "types.h"

static Cell *progn = NULL;
static LM   *lm    = NULL;

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
  ck_assert_ptr_eq (eval_res, T);

  eval_res = run_eval_progn ("(gt 5)");
  ck_assert_ptr_eq (eval_res, T);

  eval_res = run_eval_progn ("(gt 2 3)");
  ck_assert (cell_is_nil (eval_res));

  eval_res = run_eval_progn ("(gt 3 3)");
  ck_assert (cell_is_nil (eval_res));

  eval_res = run_eval_progn ("(gt 3.5 3 2.5)");
  ck_assert_ptr_eq (eval_res, T);

  eval_res = run_eval_progn ("(gt)");
  ck_assert_ptr_eq (eval_res, NIL);
}
END_TEST

START_TEST (test_lt)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(lt 1 2 3)");
  ck_assert_ptr_eq (eval_res, T);

  eval_res = run_eval_progn ("(lt 5)");
  ck_assert_ptr_eq (eval_res, T);

  eval_res = run_eval_progn ("(lt 3 2)");
  ck_assert (cell_is_nil (eval_res));

  eval_res = run_eval_progn ("(lt 4 4)");
  ck_assert (cell_is_nil (eval_res));

  eval_res = run_eval_progn ("(lt 1 1.5 2.0)");
  ck_assert_ptr_eq (eval_res, T);
}
END_TEST

START_TEST (test_add)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(+)");
  ck_assert_int_eq (eval_res->integer, 0);

  eval_res = run_eval_progn ("(add 1 2 3)");
  ck_assert_int_eq (eval_res->integer, 6);

  eval_res = run_eval_progn ("(add 5)");
  ck_assert_int_eq (eval_res->integer, 5);

  eval_res = run_eval_progn ("(add 0 -3 7)");
  ck_assert_int_eq (eval_res->integer, 4);

  eval_res = run_eval_progn ("(add 1 2.5)");
  ck_assert (cell_has_type (eval_res, CELL_TYPE_FLOAT));
  ck_assert_float_eq (eval_res->floating, 3.5);

  eval_res = run_eval_progn ("(add 9223372036854775807 1)");
  ck_assert_ptr_eq (eval_res, NIL);
}
END_TEST

START_TEST (test_sub)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(sub 10 2 3)");
  ck_assert_int_eq (eval_res->integer, 5);

  eval_res = run_eval_progn ("(sub 5)");
  ck_assert_int_eq (eval_res->integer, -5);

  eval_res = run_eval_progn ("(sub 2 5)");
  ck_assert_int_eq (eval_res->integer, -3);

  eval_res = run_eval_progn ("(sub 5.0 1 1.5)");
  ck_assert (cell_has_type (eval_res, CELL_TYPE_FLOAT));
  ck_assert_float_eq (eval_res->floating, 2.5);
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

  eval_res = run_eval_progn ("(mul 2 2.5)");
  ck_assert (cell_has_type (eval_res, CELL_TYPE_FLOAT));
  ck_assert_float_eq (eval_res->floating, 5.0);
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
  ck_assert (cell_is_nil (eval_res));

  eval_res = run_eval_progn ("(div 5 2)");
  ck_assert_int_eq (eval_res->integer, 2);

  eval_res = run_eval_progn ("(div 5 2.0)");
  ck_assert (cell_has_type (eval_res, CELL_TYPE_FLOAT));
  ck_assert_float_eq (eval_res->floating, 2.5);

  eval_res = run_eval_progn ("(div 1.0 0.0)");
  ck_assert_ptr_eq (eval_res, NIL);

  eval_res = run_eval_progn ("(div 1 'a)");
  ck_assert_ptr_eq (eval_res, NIL);
}
END_TEST

START_TEST (test_integer_literal_boundaries)
{
  char literal[64];
  snprintf (literal, sizeof (literal), "%lld", LLONG_MAX);
  Cell *value = run_eval_progn (literal);
  ck_assert (cell_has_type (value, CELL_TYPE_INTEGER));
  ck_assert_int_eq (value->integer, LLONG_MAX);

  snprintf (literal, sizeof (literal), "%lld", LLONG_MIN);
  value = run_eval_progn (literal);
  ck_assert (cell_has_type (value, CELL_TYPE_INTEGER));
  ck_assert_int_eq (value->integer, LLONG_MIN);
  ck_assert_int_eq (run_eval_progn ("-0")->integer, 0);
}
END_TEST

START_TEST (test_integer_literal_overflow)
{
  const char *invalid[]
      = { "9223372036854775808", "-9223372036854775809",
          "'9223372036854775808", "(+ 1 9223372036854775808)",
          "42 -9223372036854775809" };
  for (size_t i = 0; i < sizeof (invalid) / sizeof (*invalid); ++i)
    {
      Cell *ast = T;
      ck_assert_msg (!parser_buf (invalid[i], &ast, lm),
                     "Should reject numeric input: %s", invalid[i]);
      ck_assert_ptr_eq (ast, NIL);
    }

  char huge[512];
  memset (huge, '9', sizeof (huge) - 1);
  huge[sizeof (huge) - 1] = '\0';
  Cell *ast               = T;
  ck_assert (!parser_buf (huge, &ast, lm));
  ck_assert_ptr_eq (ast, NIL);
  // Conversion errors must not poison a subsequent parse/evaluation.
  ck_assert_int_eq (run_eval_progn ("42")->integer, 42);
}
END_TEST

START_TEST (test_float_literal_boundaries)
{
  // The reader accepts decimal notation, so print exact fixed-point values
  // instead of exponent notation. 1074 places suffice for a binary64 value.
  char        literal[1200];
  const Float values[] = { DBL_MAX, -DBL_MAX, DBL_MIN, -DBL_MIN };
  for (size_t i = 0; i < sizeof (values) / sizeof (*values); ++i)
    {
      int places = fabs (values[i]) < 1.0 ? 1074 : 1;
      int len
          = snprintf (literal, sizeof (literal), "%.*f", places, values[i]);
      ck_assert_int_gt (len, 0);
      ck_assert_uint_lt ((size_t)len, sizeof (literal));
      Cell *value = run_eval_progn (literal);
      ck_assert (cell_has_type (value, CELL_TYPE_FLOAT));
      ck_assert (isfinite (value->floating));
      ck_assert (value->floating == values[i]);
    }
  Cell *zero = run_eval_progn ("-0.0");
  ck_assert (cell_has_type (zero, CELL_TYPE_FLOAT));
  ck_assert (zero->floating == 0.0);
  ck_assert (signbit (zero->floating));
}
END_TEST

START_TEST (test_float_literal_range_errors)
{
  char overflow[512];
  memset (overflow, '9', 400);
  memcpy (overflow + 400, ".0", 3);

  char underflow[512] = "0.";
  memset (underflow + 2, '0', 400);
  memcpy (underflow + 402, "1", 2);

  const char *invalid[] = { overflow, underflow };
  for (size_t i = 0; i < sizeof (invalid) / sizeof (*invalid); ++i)
    {
      for (int negative = 0; negative <= 1; ++negative)
        {
          char literal[513];
          snprintf (literal, sizeof (literal), "%s%s", negative ? "-" : "",
                    invalid[i]);
          Cell *ast = T;
          ck_assert (!parser_buf (literal, &ast, lm));
          ck_assert_ptr_eq (ast, NIL);
          ck_assert (run_eval_progn ("1.5")->floating == 1.5);
        }
    }
}
END_TEST

START_TEST (test_numeric_range_errors_in_streams)
{
  FILE *input = tmpfile ();
  ck_assert_ptr_nonnull (input);
  fputs ("(define x 1) 9223372036854775808", input);
  rewind (input);
  Cell *ast = T;
  ck_assert (!parser_stream (input, &ast, lm));
  ck_assert_ptr_eq (ast, NIL);
  fclose (input);
  ck_assert_int_eq (run_eval_progn ("(+ 20 22)")->integer, 42);
}
END_TEST

Suite *
eval_math_suite (void)
{
  Suite *s = suite_create ("Eval Math");

  TCase *tc = tcase_create ("Core");
  tcase_add_checked_fixture (tc, setup, teardown);

  tcase_add_test (tc, test_gt);
  tcase_add_test (tc, test_lt);
  tcase_add_test (tc, test_add);
  tcase_add_test (tc, test_sub);
  tcase_add_test (tc, test_mul);
  tcase_add_test (tc, test_div);
  tcase_add_test (tc, test_integer_literal_boundaries);
  tcase_add_test (tc, test_integer_literal_overflow);
  tcase_add_test (tc, test_float_literal_boundaries);
  tcase_add_test (tc, test_float_literal_range_errors);
  tcase_add_test (tc, test_numeric_range_errors_in_streams);

  suite_add_tcase (s, tc);
  return s;
}
