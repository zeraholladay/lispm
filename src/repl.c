#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "eval.h"
#include "format.h"
#include "lispm.h"
#include "parser.h"
#include "readline.h"
#include "types.h"

#ifndef RL_BUF_SIZ
#define RL_BUF_SIZ 8192
#endif

extern char *optarg;
extern int optind;
extern int optopt;
extern int opterr;
extern int optreset;

#ifndef OBJ_POOL_CAPACITY
#define OBJ_POOL_CAPACITY 4096
#endif

void
lispm_init (LM *lm)
{
  Cell *nil = KEYWORD (NIL);
  CAR (nil) = CDR (nil) = nil;

  lm->pool = pool_init (OBJ_POOL_CAPACITY, sizeof (Cell));
  lm->env = env_create ();
}

void
lispm_destroy (LM *lm)
{
  env_destroy (lm->env);
  pool_destroy_hier (&lm->pool);
}

int
lispm_eval_progn (Cell *parse_head, LM *lm)
{
  Cell *eval_result = lispm_progn (parse_head, lm);
  PRINT (eval_result);
  return 0;
}

int
lispm_repl (LM *lm)
{
  Cell *progn = NULL;
  char input[RL_BUF_SIZ];

  rl_init ();

  for (;;)
    {
      int len = rl_readline (input, sizeof (input));

      if (len < 0)
        break;

      if (!parser_buf (input, &progn, lm))
        {
          perror ("Parse failed");
          continue;
        }

      lispm_eval_progn (progn, lm);
    }

  return 0;
}

int
lispm_main (int argc, char **argv)
{
  int run_repl = 0;
  int opt;

  while ((opt = getopt (argc, argv, "ih")) != -1)
    {
      switch (opt)
        {
        case 'i':
          run_repl = 1;
          break;
        case 'h': // fall through for now
        case '?':
        default:
          fprintf (stderr, "Usage: %s [-i] [-h] f1...\n", argv[0]);
          return 1;
        }
    }

  argc -= optind;
  argv += optind;

  if (!argc)
    run_repl = 1;

  Context lm = {};
  lispm_init (&lm);

  for (int i = 0; i < argc; ++i)
    {
      Cell *progn = NULL;
      FILE *in = fopen (argv[i], "r");

      bool res = parser_stream (in, &progn, &lm);

      fclose (in);

      if (!res)
        {
          perror ("Parse failed");
          break; // TODO: syntax error
        }

      int eval_status = lispm_eval_progn (progn, &lm);

      if (eval_status)
        {
          perror ("Eval failed.");
          break;
        }
    }

  if (run_repl)
    lispm_repl (&lm);

  return 0;
}
