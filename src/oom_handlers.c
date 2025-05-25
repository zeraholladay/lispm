#include <stdio.h>
#include <stdlib.h>

#include "oom_handlers.h"

static void
oom_handler_die (void *void_ptr, const char *msg)
{
  (void)void_ptr;
  perror (msg);
  exit (1);
  abort ();
}

oom_handler_t stack_oom_handler = oom_handler_die;
oom_handler_t palloc_oom_handler = oom_handler_die;
oom_handler_t sym_save_oom_handler = oom_handler_die;
oom_handler_t node_oom_list_handler = oom_handler_die;
oom_handler_t env_oom_handler = oom_handler_die;
oom_handler_t list_oom_handler = oom_handler_die;