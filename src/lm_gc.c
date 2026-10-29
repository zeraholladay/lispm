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
        case TYPE_CONS:
          stack_push (stack, CDR (c));
          stack_push (stack, CAR (c));
          break;

        case TYPE_LAMBDA:
          stack_push (stack, c->lambda.body);
          stack_push (stack, c->lambda.params);
          stack_push (stack, c->lambda.env);
          break;

        case TYPE_ENV:
          if (c->env.parent)
            stack_push (stack, c->env.parent);
          {
            DictIter    iter = dict_iter (c->env.dict);
            DictEntity *entity;
            while ((entity = dict_items (&iter)))
              stack_push (stack, entity->val);
          }
          break;

        case TYPE_INTEGER:
        case TYPE_FLOAT:
        case TYPE_STRING:
        case TYPE_SYMBOL:
        case TYPE_NIL:
        case TYPE_THUNK:
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
  Union u = state.u;

  switch (state.s)
    {
#define X(tag, ...)                                                           \
  case s_##tag:                                                               \
    goto gc_state_##tag;
#include "lm.def"
#undef X
    default:
      assert (0 && "no such GC state");
    }

gc_state_and_cont:
  {
    gc_mark_reachable (stack, u.and_cont.arglist);
    return;
  }

gc_state_and:
  {
    gc_mark_reachable (stack, u.and.arglist);
    return;
  }

gc_state_apply:
  {
    gc_mark_reachable (stack, u.apply.fn);
    gc_mark_reachable (stack, u.apply.arglist);
    return;
  }

gc_state_closure_enter:
  {
    gc_mark_reachable (stack, u.closure_enter.fn);
    return;
  }

gc_state_closure_leave:
  {
    gc_mark_reachable (stack, u.closure_leave.env);
    return;
  }

gc_state_define:
  {
    gc_mark_reachable (stack, u.define.sym);
    return;
  }

gc_state_eval_apply:
  {
    gc_mark_reachable (stack, u.eval_apply.fn);
    gc_mark_reachable (stack, u.eval_apply.arglist);
    return;
  }

gc_state_eval:
  {
    gc_mark_reachable (stack, u.eval.expr);
    return;
  }

gc_state_evlis_acc:
  {
    gc_mark_reachable (stack, u.evlis_acc.acc);
    gc_mark_reachable (stack, u.evlis_acc.arglist);
    return;
  }

gc_state_evlis:
  {
    gc_mark_reachable (stack, u.evlis.acc);
    gc_mark_reachable (stack, u.evlis.arglist);
    return;
  }

gc_state_funcall:
  {
    gc_mark_reachable (stack, u.funcall.fn);
    gc_mark_reachable (stack, u.funcall.arglist);
    return;
  }

gc_state_if_:
  {
    gc_mark_reachable (stack, u.if_.form);
    return;
  }

gc_state_if_cont:
  {
    gc_mark_reachable (stack, u.if_cont.form);
    return;
  }

gc_state_lambda:
  {
    gc_mark_reachable (stack, u.lambda.fn);
    gc_mark_reachable (stack, u.lambda.arglist);
    return;
  }

gc_state_let:
  {
    gc_mark_reachable (stack, u.let.arglist);
    return;
  }

gc_state_lispm:
  {
    gc_mark_reachable (stack, u.lispm.fn);
    gc_mark_reachable (stack, u.lispm.arglist);
    return;
  }

gc_state_or_cont:
  {
    gc_mark_reachable (stack, u.or_cont.arglist);
    return;
  }

gc_state_or:
  {
    gc_mark_reachable (stack, u.or.arglist);
    return;
  }

gc_state_progn_eval:
  {
    gc_mark_reachable (stack, u.progn_eval.arglist);
    return;
  }

gc_state_progn:
  {
    gc_mark_reachable (stack, u.progn.res);
    gc_mark_reachable (stack, u.progn.arglist);
    return;
  }

gc_state_set:
  {
    gc_mark_reachable (stack, u.set.sym);
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
