#ifndef STACK_H
#define STACK_H

#include <stdlib.h>

#ifndef STACK_GROWTH
#define STACK_GROWTH 4096
#endif

struct stack;
typedef struct stack Stack;

Stack *stack_create (void);
void stack_destroy (Stack *s_ptr);
void stack_push (Stack *s_ptr, void *value);
void *stack_pop (Stack *s_ptr);
void *stack_peek (Stack *s_ptr);
void stack_enter_frame (Stack *s_ptr);
void stack_exit_frame (Stack *s_ptr);

#endif
