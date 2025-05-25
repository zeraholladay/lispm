#include <stdlib.h>

#include "oom_handlers.h"
#include "stack.h"

extern oom_handler_t stack_oom_handler;

static int
stack_realloc (Stack *stack, size_t count)
{
  uintptr_t *new_ptr = realloc (stack->data, (count) * sizeof *(stack->data));
  if (!new_ptr)
    {
      return 1;
    }
  stack->data = new_ptr;
  stack->data_size = count;
  return 0;
}

void
stack_init (Stack *stack, size_t count)
{
  if (stack_realloc (stack, count))
    stack_oom_handler (stack, OOM_LOCATION);
  else
    stack->sp = stack->fp = 0;
}

void
stack_free (Stack *stack)
{
  free (stack->data), stack->data = NULL;
}

void
stack_push (Stack *stack, void *value)
{
  if (stack->sp >= stack->data_size
      && stack_realloc (stack, stack->data_size + STACK_GROWTH))
    stack_oom_handler (stack, OOM_LOCATION);
  else
    stack->data[stack->sp++] = (uintptr_t)value;
}

void *
stack_pop (Stack *stack)
{
  if (stack->sp <= stack->fp)
    return NULL;
  return (void *)stack->data[--stack->sp];
}

void *
stack_peek (Stack *stack)
{
  if (stack->sp <= stack->fp)
    return NULL;
  return (void *)(stack->data[stack->sp - 1]);
}

void
stack_enter_frame (Stack *stack)
{
  stack_push (stack, (void *)stack->fp);
  stack->fp = stack->sp;
}

void
stack_exit_frame (Stack *stack)
{
  if (stack->fp > 0)
    {
      uintptr_t old_fp = (uintptr_t)(stack->data[stack->fp - 1]);
      stack->sp = stack->fp - 1;
      stack->fp = old_fp;
    }
}
