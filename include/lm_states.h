#ifndef LM_STATES_H
#define LM_STATES_H

#include "prims.h"

// These functions construct VM instructions; they do not execute them. Keep
// state tags, union layout, and initialization defaults out of the evaluator's
// control-stack code. In particular, Lisp NIL is not the C NULL sentinel used
// to request a value from the stack.

static inline State
eval_expr (Cell *expression)
{
  return (State){ .kind = STATE_EVAL, .payload.eval = { .expr = expression } };
}

static inline State
eval_from_stack (void)
{
  return eval_expr (NULL);
}

// Evaluate a call's operator before dispatching its unevaluated argument
// forms.
static inline State
eval_call (Cell *operator_expression, Cell *arguments)
{
  return (State){ .kind = STATE_EVAL_APPLY,
                  .payload.eval_apply
                  = { .fn = operator_expression, .arglist = arguments } };
}

static inline State
continue_call (Cell *arguments)
{
  return eval_call (NULL, arguments);
}

// Arguments are accumulated in reverse order and reversed once at completion.
static inline State
eval_remaining_args (Cell *remaining, Cell *reversed)
{
  return (State){ .kind          = STATE_EVLIS,
                  .payload.evlis = { .acc = reversed, .arglist = remaining } };
}

static inline State
eval_args (Cell *arguments)
{
  return eval_remaining_args (arguments, NIL);
}

// After an argument finishes, pop its result into the reversed accumulator.
static inline State
collect_arg (Cell *remaining, Cell *reversed)
{
  return (State){ .kind = STATE_EVLIS_ACC,
                  .payload.evlis_acc
                  = { .acc = reversed, .arglist = remaining } };
}

static inline State
call_function (Cell *function, Cell *arguments)
{
  return (State){ .kind = STATE_FUNCALL,
                  .payload.funcall
                  = { .fn = function, .arglist = arguments } };
}

static inline State
call_with_stacked_args (Cell *function)
{
  return call_function (function, NULL);
}

// Both inputs were produced by earlier instructions: pop the function first,
// then the evaluated argument list. apply subsequently splices its final list.
static inline State
call_from_stack (void)
{
  return call_function (NULL, NULL);
}

static inline State
apply_from_stack (void)
{
  return (State){ .kind          = STATE_APPLY,
                  .payload.apply = { .fn = NULL, .arglist = NULL } };
}

static inline State
enter_closure (Cell *function)
{
  return (State){ .kind                  = STATE_CLOSURE_ENTER,
                  .payload.closure_enter = { .fn = function } };
}

static inline State
run_lambda (Cell *function, Cell *arguments)
{
  return (State){ .kind           = STATE_LAMBDA,
                  .payload.lambda = { .fn = function, .arglist = arguments } };
}

static inline State
return_to (Cell *environment)
{
  return (State){ .kind                  = STATE_CLOSURE_LEAVE,
                  .payload.closure_leave = { .env = environment } };
}

static inline State
define_from_stack (Cell *symbol)
{
  return (State){ .kind = STATE_DEFINE, .payload.define = { .sym = symbol } };
}

static inline State
set_from_stack (Cell *symbol)
{
  return (State){ .kind = STATE_SET, .payload.set = { .sym = symbol } };
}

static inline State
resume_body (Cell *expressions, Cell *previous_result)
{
  return (State){ .kind = STATE_PROGN,
                  .payload.progn
                  = { .res = previous_result, .arglist = expressions } };
}

static inline State
eval_body (Cell *expressions)
{
  return resume_body (expressions, NIL);
}

static inline State
continue_body (Cell *remaining)
{
  return (State){ .kind               = STATE_PROGN_EVAL,
                  .payload.progn_eval = { .arglist = remaining } };
}

static inline State
eval_special_form (Cell *function, Cell *arguments)
{
  return (State){ .kind = STATE_SPECIAL_FORM,
                  .payload.special_form
                  = { .fn = function, .arglist = arguments } };
}

static inline State
eval_if (Cell *form)
{
  return (State){ .kind = STATE_IF, .payload.if_ = { .form = form } };
}

// Consume the predicate's value and select from the unevaluated branches.
static inline State
select_branch (Cell *branches)
{
  return (State){ .kind            = STATE_IF_CONT,
                  .payload.if_cont = { .form = branches } };
}

#endif
