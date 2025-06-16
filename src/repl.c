#include <stdio.h>
#include <stdlib.h>

#include "fmt.h"
#include "lm.h"
#include "parser.h"
#include "readline.h"
#include "types.h"

#ifndef RL_BUF_SIZ
#define RL_BUF_SIZ 4096
#endif

int
repl (LM *lm)
{
  Cell *progn = NULL;
  char  input[RL_BUF_SIZ];

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

      Cell *res = lm_progn (lm, progn);

      PRINT (res);
    }

  return 0;
}
