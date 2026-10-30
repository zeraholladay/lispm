#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <readline/history.h>
#include <readline/readline.h>

#include "keywords.h"

#ifndef READLINE_HISTORY_MAX
#define READLINE_HISTORY_MAX 100
#endif

char *
keyword_completion_generator (const char *text, int state)
{
  const char *word = is_keyword_strncmp (text, state);

  if (word)
    {
      return strdup (word);
    }

  return NULL;
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
  return rl_completion_matches (text, keyword_completion_generator);
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
  const char *hist_path            = get_history_path ();
  read_history (hist_path);
  stifle_history (READLINE_HISTORY_MAX);
  rl_variable_bind ("blink-matching-paren", "on");
  atexit (rl_cleanup);
}

char *
rl_readline (void)
{
  char *line = readline ("> ");

  if (!line)
    return NULL;

  if (*line)
    {
      add_history (line);
    }

  return line;
}
