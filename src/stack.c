#include <stdlib.h>

#include "stack.h"
#include "xalloc.h"

struct stack
{
  uintptr_t sp;
  uintptr_t fp;
  size_t data_size;
  uintptr_t *data;
};

static void *
stack_xgrow (Stack *stack, size_t count)
{
  uintptr_t *new_ptr = xrealloc (stack->data, (count) * sizeof *(stack->data));
  stack->data = new_ptr;
  stack->data_size = count;
  return new_ptr;
}

Stack *
stack_create (void)
{
  Stack *stack = xmalloc (sizeof *(stack));
  stack_xgrow (stack, 0);
  stack->sp = stack->fp = 0;
  return stack;
}

void
stack_destroy (Stack *stack)
{
  free (stack->data), stack->data = NULL;
  free (stack);
}

void
stack_push (Stack *stack, void *value)
{
  if (stack->sp >= stack->data_size)
    stack_xgrow (stack, stack->data_size + STACK_GROWTH);
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
