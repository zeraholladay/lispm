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
  char input[32];

  next_line = "(+ 1 2)";
  ck_assert_int_eq (rl_readline (input, sizeof (input)), 7);
  ck_assert_str_eq (input, "(+ 1 2)");
}
END_TEST

Suite *
readline_suite (void)
{
  Suite *s = suite_create ("Readline");
  TCase *t = tcase_create ("Core");

  tcase_add_test (t, test_readline_returns_one_submitted_line);
  suite_add_tcase (s, t);
  return s;
}
