#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include <readline/history.h>
#include <readline/readline.h>

#include "keywords.h"
#include "readline.h"
#include "types.h"

#ifndef READLINE_HISTORY_MAX
#define READLINE_HISTORY_MAX 100
#endif

// Readline callbacks carry no user data. Borrow the VM only while reading or
// collecting matches, never across evaluation or interpreter destruction.
static LM *completion_lm;

static char *
symbol_completion_generator (const char *text, int state)
{
  static int      keyword_state;
  static bool     keywords_done;
  static Cell    *frame;
  static DictIter iter;
  size_t          length = strlen (text);

  if (state == 0)
    {
      keyword_state = 0;
      keywords_done = false;
      frame         = completion_lm ? completion_lm->env.current : NULL;
      if (frame)
        iter = dict_iter (frame->env.dict);
      // Lambda is recognized directly by the lexer, outside the keyword table.
      if (!strncasecmp ("lambda", text, length))
        return strdup ("lambda");
    }

  if (!keywords_done)
    {
      const char *word = is_keyword_strncmp (text, keyword_state++);
      if (word)
        return strdup (word);
      keywords_done = true;
    }

  while (frame)
    {
      DictEntity *entry;
      while ((entry = dict_items (&iter)))
        if (!strncmp (entry->key, text, length))
          return strdup (entry->key);
      frame = frame->env.parent;
      if (frame)
        iter = dict_iter (frame->env.dict);
    }

  return NULL;
}

static int
compare_matches (const void *left, const void *right)
{
  return strcmp (*(char *const *)left, *(char *const *)right);
}

char **
rl_symbol_matches (LM *lm, const char *text)
{
  LM *saved      = completion_lm;
  completion_lm  = lm;
  char **matches = rl_completion_matches (text, symbol_completion_generator);
  completion_lm  = saved;

  if (!matches || !matches[1])
    return matches;

  // matches[0] is Readline's common-prefix replacement. Sort/deduplicate only
  // the alternatives, keeping that contract and Readline's string ownership.
  size_t count = 0;
  while (matches[count + 1])
    ++count;

  qsort (matches + 1, count, sizeof (*matches), compare_matches);

  size_t used = 1;
  for (size_t i = 2; i <= count; ++i)
    if (!strcmp (matches[used], matches[i]))
      free (matches[i]);
    else
      matches[++used] = matches[i];

  matches[used + 1] = NULL;

  if (used == 1)
    {
      free (matches[0]);
      matches[0] = matches[1];
      matches[1] = NULL;
    }

  return matches;
}

static const char *
get_history_path (void)
{
  const char *home = getenv ("HOME");
  static char path[PATH_MAX];
  snprintf (path, sizeof (path), "%s/.lispm_history", home ? home : ".");
  return path;
}

static char **
_attempted_completion_function (const char *text, int start, int end)
{
  (void)start;
  (void)end;
  rl_attempted_completion_over = 1;
  return rl_symbol_matches (completion_lm, text);
}

static void
rl_cleanup (void)
{
  const char *hist_path = get_history_path ();
  write_history (hist_path);
}

void
rl_init (void)
{
  rl_attempted_completion_function = _attempted_completion_function;
  // Lisp delimiters end a name; hyphens, predicate ?, mutation !, and operator
  // characters remain part of it. Use our own boundaries rather than the
  // shell-oriented Readline defaults (which split names such as set!).
  rl_completer_word_break_characters = " \t\r\n()[]'\";";
  rl_completer_quote_characters      = "";
  // Lisp quote prefixes one form; it is not a paired shell quotation. Without
  // this, Readline appends a closing apostrophe after completing quoted data.
  rl_basic_quote_characters = "";
  rl_special_prefixes       = NULL;
  const char *hist_path     = get_history_path ();
  read_history (hist_path);
  stifle_history (READLINE_HISTORY_MAX);
  rl_variable_bind ("blink-matching-paren", "on");
  atexit (rl_cleanup);
}

char *
rl_readline (LM *lm)
{
  completion_lm = lm;
  char *line    = readline ("> ");
  completion_lm = NULL;

  if (!line)
    return NULL;

  if (*line)
    {
      add_history (line);
    }

  return line;
}
