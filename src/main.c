#ifdef LISPM_MAIN

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "fmt.h"
#include "parser.h"
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
      char *fname = argv[i];

      void *bytes = parser_load (fname);
      if (!bytes)
        {
          fprintf (stderr, "Parse failed to load input file: %s\n", fname);
          continue;
        }

      bool parse_res = parser_parse_bytes (bytes, &progn, lm);

      if (!parse_res)
        {
          fprintf (stderr, "Parse failed on %s\n", fname);
          return 1;
        }

      lm_progn (lm, progn);
    }

  if (run_repl)
    repl (lm);

  return 0;
}

int
main (int argc, char **argv)
{
  atexit (parser_destroy);
  return run (argc, argv);
}

#endif
