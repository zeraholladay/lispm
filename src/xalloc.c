#include <stdio.h>
#include <stdlib.h>

#include "xalloc.h"

_Noreturn void
xalloc_die (const char *message)
{
  fprintf (stderr, "%s: allocation failed\n", message);
  exit (EXIT_FAILURE);
}
