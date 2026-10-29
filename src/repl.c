#include <stdio.h>
#include <stdlib.h>

#include "fmt.h"
#include "lm.h"
#include "parser.h"
#include "readline.h"
#include "types.h"

int
repl (LM *lm)
{
  Cell *progn = NULL;

  rl_init ();

  for (;;)
    {
      char *input = rl_readline ();

      if (!input)
        break;

      bool parsed = parser_buf (input, &progn, lm);
      free (input);
      if (!parsed)
        {
          fputs ("Parse failed\n", stderr);
          continue;
        }

      Cell *res = lm_progn (lm, progn);

      PRINT (res);
    }

  return 0;
}
