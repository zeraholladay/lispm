#include <check.h>
#include <stdio.h>
#include <string.h>

#include <readline/readline.h>

#include "parser.h"
#include "prims.h"
#include "readline.h"

static const char *next_line;
static const char *completion_prefix;
static char      **submitted_matches;

// Keep Readline initialization tests independent of the user's history file.
int
read_history (const char *path)
{
  (void)path;
  return 0;
}

int
write_history (const char *path)
{
  (void)path;
  return 0;
}

char *
readline (const char *prompt)
{
  (void)prompt;

  if (!next_line)
    return NULL;

  if (completion_prefix)
    submitted_matches = rl_attempted_completion_function (
        completion_prefix, 1, (int)strlen (completion_prefix) + 1);
  char *line = strdup (next_line);
  next_line  = NULL;
  return line;
}

START_TEST (test_readline_returns_one_submitted_line)
{
  next_line   = "(+ 1 2)";
  char *input = rl_readline (NULL);
  ck_assert_ptr_nonnull (input);
  ck_assert_str_eq (input, "(+ 1 2)");
  free (input);
  ck_assert_ptr_null (rl_readline (NULL));
}
END_TEST

START_TEST (test_readline_long_line)
{
  char line[8193];
  memset (line, 'a', sizeof (line) - 1);
  line[sizeof (line) - 1] = '\0';
  next_line               = line;
  char *input             = rl_readline (NULL);
  ck_assert_ptr_nonnull (input);
  ck_assert_str_eq (input, line);
  free (input);
}
END_TEST

static void
free_matches (char **matches)
{
  if (!matches)
    return;
  for (size_t i = 0; matches[i]; ++i)
    free (matches[i]);
  free (matches);
}

static void
assert_unique_match (LM *lm, const char *prefix, const char *expected)
{
  char **matches = rl_symbol_matches (lm, prefix);
  ck_assert_ptr_nonnull (matches);
  ck_assert_str_eq (matches[0], expected);
  ck_assert_ptr_null (matches[1]);
  free_matches (matches);
}

static void
evaluate (LM *lm, const char *source)
{
  Cell *forms;
  ck_assert (parser_buf (source, &forms, lm));
  ck_assert (lm_evaluate (lm, forms).success);
}

START_TEST (test_completion_live_bindings)
{
  LM *lm = lm_create ();
  ck_assert_ptr_nonnull (lm);
  assert_unique_match (lm, "fold-l", "fold-left");
  ck_assert_ptr_null (rl_symbol_matches (lm, "completion-counter"));
  evaluate (lm, "(define completion-counter 0)"
                "(define (completion-square x) (* x x))");
  assert_unique_match (lm, "completion-c", "completion-counter");
  assert_unique_match (lm, "completion-s", "completion-square");
  evaluate (lm, "(set! completion-counter 42)"
                "(define completion-square 9)");
  assert_unique_match (lm, "completion-c", "completion-counter");
  assert_unique_match (lm, "completion-s", "completion-square");
  // Environment names are case-sensitive, just like runtime lookup.
  ck_assert_ptr_null (rl_symbol_matches (lm, "Completion-"));
  lm_destroy (lm);
  // No callback may keep a destroyed interpreter alive or consult it later.
  ck_assert_ptr_null (rl_symbol_matches (NULL, "completion-c"));
}
END_TEST

START_TEST (test_completion_sorted_and_deduplicated)
{
  LM *lm = lm_create ();
  ck_assert_ptr_nonnull (lm);
  evaluate (lm, "(define completion-z 1) (define completion-a 2)");
  lm->env.current = cell_make_environment (lm, lm->env.current);
  evaluate (lm, "(define completion-a 3) (define completion-m 4)");
  char **matches = rl_symbol_matches (lm, "completion-");
  ck_assert_ptr_nonnull (matches);
  ck_assert_str_eq (matches[0], "completion-");
  ck_assert_str_eq (matches[1], "completion-a");
  ck_assert_str_eq (matches[2], "completion-m");
  ck_assert_str_eq (matches[3], "completion-z");
  ck_assert_ptr_null (matches[4]);
  free_matches (matches);
  assert_unique_match (lm, "completion-a", "completion-a");
  // A prelude name shadowed in a nearer frame must appear just once too.
  evaluate (lm, "(define fold-left 0)");
  assert_unique_match (lm, "fold-l", "fold-left");
  lm_destroy (lm);
}
END_TEST

START_TEST (test_completion_keywords_and_operators)
{
  const char *names[]
      = { "lambda", "quote",      "define",  "if", "let", "set!",
          "pair?",  "procedure?", "symbol?", "+",  "-",   "*",
          "/",      "<",          ">",       "=",  "nil", "t" };
  // Empty-prefix completion must enumerate the whole sparse keyword table.
  char **matches = rl_symbol_matches (NULL, "");
  ck_assert_ptr_nonnull (matches);
  for (size_t n = 0; n < sizeof (names) / sizeof (names[0]); ++n)
    {
      size_t found = 0;
      for (size_t i = 1; matches[i]; ++i)
        if (!strcmp (matches[i], names[n]))
          ++found;
      ck_assert_uint_eq (found, 1);
    }
  for (size_t i = 2; matches[i]; ++i)
    ck_assert_int_lt (strcmp (matches[i - 1], matches[i]), 0);
  free_matches (matches);
  assert_unique_match (NULL, "SET!", "set!");
  assert_unique_match (NULL, "pair?", "pair?");
  ck_assert_ptr_null (rl_symbol_matches (NULL, "no-such-completion"));
}
END_TEST

START_TEST (test_readline_completion_context_and_boundaries)
{
  LM *lm = lm_create ();
  ck_assert_ptr_nonnull (lm);
  evaluate (lm, "(define completion-live 42)");
  rl_init ();
  const char *breaks = "()[]'\"; \t\r\n";
  for (size_t i = 0; breaks[i]; ++i)
    ck_assert_ptr_nonnull (
        strchr (rl_completer_word_break_characters, breaks[i]));
  const char *inside = "-?!+*/<>=_";
  for (size_t i = 0; inside[i]; ++i)
    ck_assert_ptr_null (
        strchr (rl_completer_word_break_characters, inside[i]));
  ck_assert_str_eq (rl_completer_quote_characters, "");
  ck_assert_str_eq (rl_basic_quote_characters, "");
  next_line         = "[completion-live]";
  completion_prefix = "completion-l";
  char *line        = rl_readline (lm);
  free (line);
  ck_assert_ptr_nonnull (submitted_matches);
  ck_assert_str_eq (submitted_matches[0], "completion-live");
  free_matches (submitted_matches);
  submitted_matches = NULL;
  ck_assert_int_eq (rl_attempted_completion_over, 1);
  lm_destroy (lm);
  // The attempted-completion callback no longer borrows the VM after reading.
  ck_assert_ptr_null (
      rl_attempted_completion_function ("completion-l", 0, 12));
  completion_prefix = NULL;
}
END_TEST

Suite *
readline_suite (void)
{
  Suite *s = suite_create ("Readline");
  TCase *t = tcase_create ("Core");

  tcase_add_test (t, test_readline_returns_one_submitted_line);
  tcase_add_test (t, test_readline_long_line);
  tcase_add_test (t, test_completion_live_bindings);
  tcase_add_test (t, test_completion_sorted_and_deduplicated);
  tcase_add_test (t, test_completion_keywords_and_operators);
  tcase_add_test (t, test_readline_completion_context_and_boundaries);
  suite_add_tcase (s, t);
  return s;
}
