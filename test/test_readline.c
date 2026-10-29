#include <check.h>
#include <string.h>

#include "readline.h"

static const char *next_line;

char *
readline (const char *prompt)
{
  (void)prompt;

  if (!next_line)
    return NULL;

  char *line = strdup (next_line);
  next_line  = NULL;
  return line;
}

START_TEST (test_readline_returns_one_submitted_line)
{
  next_line   = "(+ 1 2)";
  char *input = rl_readline ();
  ck_assert_ptr_nonnull (input);
  ck_assert_str_eq (input, "(+ 1 2)");
  free (input);
  ck_assert_ptr_null (rl_readline ());
}
END_TEST

START_TEST (test_readline_long_line)
{
  char line[8193];
  memset (line, 'a', sizeof (line) - 1);
  line[sizeof (line) - 1] = '\0';
  next_line               = line;
  char *input             = rl_readline ();
  ck_assert_ptr_nonnull (input);
  ck_assert_str_eq (input, line);
  free (input);
}
END_TEST

Suite *
readline_suite (void)
{
  Suite *s = suite_create ("Readline");
  TCase *t = tcase_create ("Core");

  tcase_add_test (t, test_readline_returns_one_submitted_line);
  tcase_add_test (t, test_readline_long_line);
  suite_add_tcase (s, t);
  return s;
}
