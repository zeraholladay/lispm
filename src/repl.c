#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "eval.h"
#include "parser.h"
#include "readline.h"
#include "sym_save.h"
#include "types.h"

extern char *optarg;
extern int optind;
extern int optopt;
extern int opterr;
extern int optreset;

#ifndef OBJ_POOL_CAPACITY
#define OBJ_POOL_CAPACITY 4096
#endif

#if YYDEBUG
extern int yydebug;
#endif

#ifndef lispm_MAIN

#ifndef REPL_BUF_SIZ
#define REPL_BUF_SIZ 8192
#endif

extern Node *const const_false;
extern Node *const const_true;

extern FILE *yyin;
extern int yyparse (Context *ctx);
extern void yylex_destroy (void);

extern jmp_buf eval_error_jmp;

void
lispm_init (Context *ctx)
{
  Node *nil = KEYWORD (NIL);
  FIRST (nil) = REST (nil) = nil;

  static int sym_save_bool = 0;

  if (!sym_save_bool && (sym_save_bool = 1))
    sym_save_init ();

  CTX_POOL (ctx) = pool_init (OBJ_POOL_CAPACITY, sizeof (Node));
  CTX_ENV (ctx) = env_new (NULL);
  reset_parse_context (ctx);
}

void
lispm_destroy (Context *ctx)
{
  reset_parse_context (ctx);
  free (CTX_ENV (ctx)), CTX_ENV (ctx) = NULL;
  pool_destroy_hier (&CTX_POOL (ctx));
}

int
lispm_eval_program (Context *ctx)
{
  if (setjmp (eval_error_jmp) == 0)
    {
      Node *eval_result = eval_program (CTX_PARSE_ROOT (ctx), ctx);
      Node *node = eval_str (eval_result, ctx);
      printf ("%s\n", GET_STRING (node));
      free (node->as.string); // FIXME with GC
      return 0;
    }

  return 1;
}

int
lispm_repl (Context *ctx)
{
  rl_init ();

  char full_input[REPL_BUF_SIZ];

  for (;;)
    {
      int len = rl_readline (full_input, sizeof (full_input));

      if (len < 0)
        {
          break; // TODO: Something on error
        }

      yyin = fmemopen ((void *)full_input, len, "r");

      reset_parse_context (ctx);
      int parse_status = yyparse (ctx);

      yylex_destroy ();
      fclose (yyin);

      if (parse_status)
        {
          perror ("Parse failed");
          continue; // TODO: syntax error
        }

      lispm_eval_program (ctx);
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
      yyin = fopen (argv[i], "r");

      if (!yyin)
        {
          perror ("fopen");
          return 1;
        }

      int parse_status = yyparse (&ctx);

      fclose (yyin);

      if (parse_status)
        {
          perror ("Parse failed");
          break; // TODO: syntax error
        }

      int eval_status = lispm_eval_program (&ctx);

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

#else

#include "repl.h"

int
main (int argc, char **argv)
{
#if YYDEBUG
  yydebug = YYDEBUG;
#endif
  return lispm_main (argc, argv);
}

#endif