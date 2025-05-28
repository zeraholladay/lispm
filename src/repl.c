#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "eval.h"
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

extern jmp_buf eval_error_jmp;

void
lispm_init (Context *ctx)
{
  Node *nil = KEYWORD (NIL);
  CAR (nil) = CDR (nil) = nil;

  CTX_POOL (ctx) = pool_init (OBJ_POOL_CAPACITY, sizeof (Node));
  ctx->env = env_create ();
}

void
lispm_destroy (Context *ctx)
{
  env_destroy (ctx->env);
  pool_destroy_hier (&CTX_POOL (ctx));
}

int
lispm_eval_progn (Node *parse_head, Context *ctx)
{
  if (setjmp (eval_error_jmp) == 0)
    {
      Node *eval_result = eval_progn (parse_head, ctx);
      Node *node = eval_str (eval_result, ctx);
      printf ("%s\n", GET_STRING (node));
      free (node->string); // FIXME with GC
      return 0;
    }

  return 1;
}

int
lispm_repl (Context *ctx)
{
  Node *progn = NULL;
  char input[RL_BUF_SIZ];

  rl_init ();

  for (;;)
    {
      int len = rl_readline (input, sizeof (input));

      if (len < 0)
        break;

      if (!parser_buf (input, &progn, ctx))
        {
          perror ("Parse failed");
          continue;
        }

      lispm_eval_progn (progn, ctx);
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

  Context ctx = {};
  lispm_init (&ctx);

  for (int i = 0; i < argc; ++i)
    {
      Node *progn = NULL;
      FILE *in = fopen (argv[i], "r");

      bool res = parser_stream (in, &progn, &ctx);

      fclose (in);

      if (!res)
        {
          perror ("Parse failed");
          break; // TODO: syntax error
        }

      int eval_status = lispm_eval_progn (progn, &ctx);

      if (eval_status)
        {
          perror ("Eval failed.");
          break;
        }
    }

  if (run_repl)
    lispm_repl (&ctx);

  return 0;
}
