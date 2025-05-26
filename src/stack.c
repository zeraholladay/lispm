#include <stdlib.h>

#include "stack.h"
#include "xalloc.h"

static void *
stack_xrealloc (Stack *stack, size_t count)
{
  uintptr_t *new_ptr = xrealloc (stack->data, (count) * sizeof *(stack->data));
  stack->data = new_ptr;
  stack->data_size = count;
  return new_ptr;
}

void *
stack_xalloc (Stack *stack, size_t count)
{
  stack_xrealloc (stack, count);
  stack->sp = stack->fp = 0;
  return stack;
}

void
stack_free (Stack *stack)
{
  free (stack->data), stack->data = NULL;
}

void
stack_push (Stack *stack, void *value)
{
  if (stack->sp >= stack->data_size)
    stack_xrealloc (stack, stack->data_size + STACK_GROWTH);
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
