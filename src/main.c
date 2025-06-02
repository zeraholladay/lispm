#ifdef LISPM_MAIN

#include <stdio.h>
#include <unistd.h>

#include "format.h"
#include "parser.h"
#include "repl.h"

extern char *optarg;
extern int optind;
extern int optopt;
extern int opterr;
extern int optreset;

#include "repl.h"

static int
run (int argc, char **argv)
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

  LM *lm = lm_create ();

  for (int i = 0; i < argc; ++i)
    {
      Cell *progn = NULL;

      FILE *in = fopen (argv[i], "r");

      bool parse_res = parser_stream (in, &progn, lm);

      fclose (in);

      if (!parse_res)
        {
          perror ("Parse failed");
          break; // TODO: syntax error
        }

      Cell *eval_res = lm_progn (lm, progn);

      // if ()
      //   {
      //     perror ("Eval failed.");
      //     break;
      //   }
    }

  if (run_repl)
    repl (lm);

  return 0;
}

int
main (int argc, char **argv)
{
  return run (argc, argv);
}

#endif
