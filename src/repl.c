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
      char *input = rl_readline (lm);

      if (!input)
        break;

      bool parsed = parser_buf_named ("<repl>", input, &progn, lm);
      free (input);
      if (!parsed)
        {
          continue;
        }

      EvaluationResult result = lm_evaluate (lm, progn);
      if (result.success)
        PRINT (result.value);
    }

  return 0;
}
