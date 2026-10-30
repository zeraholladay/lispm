#include <assert.h>
#include <stdlib.h>

#include "lm_gc.h"
#include "palloc.h"
#include "prims.h"
#include "stack.h"

static void
gc_mark_reachable (Stack *stack, Cell *root)
{
  if (!root)
    return;

  Cell *c;
  stack_push (stack, root);

  while ((c = stack_pop (stack)))
    {
      if (pool_gc_is_marked (c))
        continue;

      pool_gc_mark (c);

      switch (c->type)
        {
        case CELL_TYPE_CONS:
          stack_push (stack, CDR (c));
          stack_push (stack, CAR (c));
          break;

        case CELL_TYPE_LAMBDA:
          stack_push (stack, c->lambda.body);
          stack_push (stack, c->lambda.params);
          stack_push (stack, c->lambda.env);
          break;

        case CELL_TYPE_ENV:
          if (c->env.parent)
            stack_push (stack, c->env.parent);
          {
            DictIter    iter = dict_iter (c->env.dict);
            DictEntity *entity;
            while ((entity = dict_items (&iter)))
              stack_push (stack, entity->val);
          }
          break;

        case CELL_TYPE_INTEGER:
        case CELL_TYPE_FLOAT:
        case CELL_TYPE_STRING:
        case CELL_TYPE_SYMBOL:
        case CELL_TYPE_NIL:
        case CELL_TYPE_BUILTIN:
          break;

        default:
          assert (0 && "no such GC type");
          break;
        }
    }
}

static void
gc_mark_state (Stack *stack, State state)
{
  StatePayload payload = state.payload;

  switch (state.kind)
    {
#define X(id, tag, ...)                                                       \
  case STATE_##id:                                                            \
    goto gc_state_##tag;
#include "lm.def"
#undef X
    default:
      assert (0 && "no such GC state");
    }

gc_state_apply:
  {
    gc_mark_reachable (stack, payload.apply.fn);
    gc_mark_reachable (stack, payload.apply.arglist);
    return;
  }

gc_state_closure_enter:
  {
    gc_mark_reachable (stack, payload.closure_enter.fn);
    return;
  }

gc_state_closure_leave:
  {
    gc_mark_reachable (stack, payload.closure_leave.env);
    return;
  }

gc_state_define:
  {
    gc_mark_reachable (stack, payload.define.sym);
    return;
  }

gc_state_eval_apply:
  {
    gc_mark_reachable (stack, payload.eval_apply.fn);
    gc_mark_reachable (stack, payload.eval_apply.arglist);
    return;
  }

gc_state_eval:
  {
    gc_mark_reachable (stack, payload.eval.expr);
    return;
  }

gc_state_evlis_acc:
  {
    gc_mark_reachable (stack, payload.evlis_acc.acc);
    gc_mark_reachable (stack, payload.evlis_acc.arglist);
    return;
  }

gc_state_evlis:
  {
    gc_mark_reachable (stack, payload.evlis.acc);
    gc_mark_reachable (stack, payload.evlis.arglist);
    return;
  }

gc_state_funcall:
  {
    gc_mark_reachable (stack, payload.funcall.fn);
    gc_mark_reachable (stack, payload.funcall.arglist);
    return;
  }

gc_state_if_:
  {
    gc_mark_reachable (stack, payload.if_.form);
    return;
  }

gc_state_if_cont:
  {
    gc_mark_reachable (stack, payload.if_cont.form);
    return;
  }

gc_state_lambda:
  {
    gc_mark_reachable (stack, payload.lambda.fn);
    gc_mark_reachable (stack, payload.lambda.arglist);
    return;
  }

gc_state_special_form:
  {
    gc_mark_reachable (stack, payload.special_form.fn);
    gc_mark_reachable (stack, payload.special_form.arglist);
    return;
  }

gc_state_progn_eval:
  {
    gc_mark_reachable (stack, payload.progn_eval.arglist);
    return;
  }

gc_state_progn:
  {
    gc_mark_reachable (stack, payload.progn.res);
    gc_mark_reachable (stack, payload.progn.arglist);
    return;
  }

gc_state_set:
  {
    gc_mark_reachable (stack, payload.set.sym);
    return;
  }
}

static void
gc_mark (LM *lm)
{
  Stack *stack = stack_create ();

  for (size_t i = 0; i < lm->stk.sp; ++i)
    gc_mark_reachable (stack, lm->stk.cells[i]);

  gc_mark_reachable (stack, lm->env.current);
  gc_mark_reachable (stack, lm->env.global);

  for (size_t i = 0; i < lm->ctl.sp; ++i)
    gc_mark_state (stack, lm->ctl.states[i]);

  stack_destroy (stack);
}

static void
gc_sweep (Pool *p, void *ptr)
{
  Cell *c = ptr;

  if (!pool_gc_is_free (c) && !pool_gc_is_marked (c))
    {
      cell_dispose (c);
      pool_free (p, c);
    }
}

static void
gc_unmark (Pool *p, void *ptr)
{
  (void)p;
  pool_gc_unmark (ptr);
}

void
lm_gc (LM *lm)
{
  pool_map_hier (lm->pool, gc_unmark);
  gc_mark (lm);
  pool_map_hier (lm->pool, gc_sweep);
}
