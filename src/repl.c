#include <stdio.h>
#include <stdlib.h>

#include "lisp_fmt.h"
#include "lisp_mach.h"
#include "lisp_headers.h"
#include "lisp_types.h"
#include "parser.h"
#include "readline.h"

#ifndef RL_BUF_SIZ
#define RL_BUF_SIZ 8192
#endif

int
repl (LM *lm)
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

      Cell *eval_result = lm_progn (lm, progn);
      PRINT (eval_result);
    }

  return 0;
}
