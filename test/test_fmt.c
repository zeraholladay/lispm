#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "builtins.h"
#include "fmt.h"
#include "parser.h"
#include "prims.h"

static LM *lm;

static void
setup (void)
{
  lm = lm_create ();
  ck_assert_ptr_nonnull (lm);
}

static void
teardown (void)
{
  lm_destroy (lm);
}

static void
expect_format (Cell *value, const char *expected)
{
  char *actual = format (value);
  ck_assert_str_eq (actual, expected);
  free (actual);
}

START_TEST (test_string_formatting_grows_buffer_and_preserves_contents)
{
  char text[4097];
  memset (text, 'x', sizeof (text) - 1);
  memcpy (text, "100%\n", 5);
  text[sizeof (text) - 1] = '\0';
  expect_format (cell_make_string_copy (lm, text), text);
  expect_format (cell_make_string_copy (lm, ""), "");
  expect_format (cell_list2 (lm, cell_make_string_copy (lm, "hello"),
                             cell_make_integer (lm, 42)),
                 "(hello 42)");
}
END_TEST

START_TEST (test_builtin_and_lambda_formatting)
{
  expect_format (builtin_value (BUILTIN_ADD), "#<BUILTIN ADD>");
  Cell *forms = NULL;
  ck_assert (
      parser_buf ("(lambda (x . extras) (+ x (car extras)))", &forms, lm));
  EvaluationResult result = lm_evaluate (lm, forms);
  ck_assert (result.success);
  expect_format (result.value, "#<LAMBDA (x . extras) ((+ x (car extras)))>");
}
END_TEST

START_TEST (test_unknown_values_have_diagnostic_representation)
{
  Cell unknown = { .type = CELL_TYPE_UNKNOWN };
  char expected[80];
  snprintf (expected, sizeof (expected), "#<UNKNOWN %p>", (void *)&unknown);
  expect_format (&unknown, expected);
  unknown.type = CELL_TYPE_COUNT;
  expect_format (&unknown, expected);
  snprintf (expected, sizeof (expected), "#<UNKNOWN %p>", (void *)NULL);
  expect_format (NULL, expected);
  Cell *environment = cell_make_environment (lm, NULL);
  snprintf (expected, sizeof (expected), "#<UNKNOWN %p>", (void *)environment);
  expect_format (environment, expected);
}
END_TEST

START_TEST (test_print_writes_one_line_and_returns_true)
{
  Cell *forms = NULL;
  ck_assert (parser_buf ("(print '(1 (2 . 3) ()))", &forms, lm));
  FILE *output = tmpfile ();
  ck_assert_ptr_nonnull (output);
  fflush (stdout);
  int saved = dup (STDOUT_FILENO);
  ck_assert_int_ge (saved, 0);
  ck_assert_int_ge (dup2 (fileno (output), STDOUT_FILENO), 0);
  EvaluationResult result = lm_evaluate (lm, forms);
  fflush (stdout);
  int restored = dup2 (saved, STDOUT_FILENO);
  close (saved);
  ck_assert_int_ge (restored, 0);
  ck_assert (result.success);
  ck_assert_ptr_eq (result.value, T);
  rewind (output);
  char   text[128];
  size_t count = fread (text, 1, sizeof (text) - 1, output);
  text[count]  = '\0';
  ck_assert (!ferror (output));
  ck_assert (feof (output));
  fclose (output);
  ck_assert_str_eq (text, "(1 (2 . 3) nil)\n");
}
END_TEST

Suite *
fmt_suite (void)
{
  Suite *suite = suite_create ("Formatting");
  TCase *tc    = tcase_create ("Values and output");
  tcase_add_checked_fixture (tc, setup, teardown);
  tcase_add_test (tc,
                  test_string_formatting_grows_buffer_and_preserves_contents);
  tcase_add_test (tc, test_builtin_and_lambda_formatting);
  tcase_add_test (tc, test_unknown_values_have_diagnostic_representation);
  tcase_add_test (tc, test_print_writes_one_line_and_returns_true);
  suite_add_tcase (suite, tc);
  return suite;
}
