#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "ctx.h"
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
extern int yyparse (Node *ast_root, Ctx *ctx);
extern void yylex_destroy (void);

extern jmp_buf eval_error_jmp;

Ctx *
lispm_init ()
{
  Node *nil = KEYWORD (NIL);
  CAR (nil) = CDR (nil) = nil;
  return ctx_create ();
}

void
lispm_destroy (Ctx *ctx)
{
  ctx_destroy (ctx);
}

int
lispm_eval_progn (Node *progn, Ctx *ctx)
{
  if (setjmp (eval_error_jmp) == 0)
    {
      Node *eval_result = eval_progn (progn, ctx);
      Node *node = eval_str (eval_result, ctx);
      printf ("%s\n", GET_STRING (node));
      free (node->string); // FIXME with GC
      return 0;
    }
  return 1;
}

void
lispm_repl (Ctx *ctx)
{
  Node *progn = NIL;
  rl_init ();

  char full_input[REPL_BUF_SIZ];

  for (;;)
    {
      int len = rl_readline (full_input, sizeof (full_input));

      if (len < 0)
        break; // TODO: Something on error

      yyin = fmemopen ((void *)full_input, len, "r");

      int parse_status = yyparse (progn, ctx);

      yylex_destroy ();
      fclose (yyin);

      if (parse_status)
        {
          perror ("Parse failed");
          continue; // TODO: syntax error
        }
      else
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

  Ctx *ctx = lispm_init ();

  for (int i = 0; i < argc; ++i)
    {
      yyin = fopen (argv[i], "r");

      if (!yyin)
        {
          perror ("fopen");
          return 1;
        }

      Node *progn = NIL;
      int parse_status = yyparse (progn, ctx);

      fclose (yyin);

      if (parse_status)
        {
          perror ("Parse failed");
          break; // TODO: syntax error
        }

      int eval_status = lispm_eval_progn (progn, ctx);

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