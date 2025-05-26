#include <stdio.h>
#include <stdlib.h>

#include "xalloc.h"

void *
oom_handler_die (void *void_ptr, const char *msg)
{
  (void)void_ptr;
  perror (msg);
  exit (1);
  abort ();
  return NULL;
}
