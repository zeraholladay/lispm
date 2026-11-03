#include <assert.h>
#include <stdlib.h>

#include "lm_gc.h"
#include "palloc.h"
#include "prims.h"
#include "stack.h"

// stack_pop returns NULL when the stack is empty, so a NULL child must never
// be pushed: it would end the mark loop early and leave cells unmarked.
static void
gc_push (Stack *stack, Cell *c)
{
  if (c)
    stack_push (stack, c);
}

static void
gc_mark_reachable (Stack *stack, Cell *root)
{
  Cell *c;
  gc_push (stack, root);

  while ((c = stack_pop (stack)))
    {
      if (pool_gc_is_marked (c))
        continue;

      pool_gc_mark (c);

      switch (c->type)
        {
        case CELL_TYPE_CONS:
          gc_push (stack, CDR (c));
          gc_push (stack, CAR (c));
          break;

        case CELL_TYPE_LAMBDA:
          gc_push (stack, c->lambda.body);
          gc_push (stack, c->lambda.params);
          gc_push (stack, c->lambda.env);
          break;

        case CELL_TYPE_ENV:
          gc_push (stack, c->env.parent);
          {
            DictIter    iter = dict_iter (c->env.dict);
            DictEntity *entity;
            while ((entity = dict_items (&iter)))
              gc_push (stack, entity->val);
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

  size_t capacity = 0;
  size_t live     = 0;
  for (Pool *pool = lm->pool; pool; pool = pool->next)
    {
      capacity += pool->count;
      live += pool->allocated_count;
    }
  // Allow one live heap's worth of new cells before collecting again. Sweeping
  // scans unused slots too, and pools are retained after a transient spike, so
  // also allow at least half the reserved capacity to amortize that scan cost.
  size_t budget = live > capacity / 2 ? live : capacity / 2;
  lm->gc.allocation_budget
      = budget > LM_GC_MIN_ALLOCATIONS ? budget : LM_GC_MIN_ALLOCATIONS;
  lm->gc.allocations_since_collection = 0;
  lm->gc.live_cells                   = live;
  ++lm->gc.collections;
}
