#include <assert.h>

#include "lm_gc.h"
#include "lm_secd.h"
#include "palloc.h"
#include "prims.h"
#include "stack.h"

static void
gc_mark_reachable (Stack *stack, Cell *root)
{
  stack_push (stack, root);

  while (true)
    {
      Cell *c = stack_pop (stack);
      if (!c)
        break;

      if (pool_gc_is_marked (c))
        return;

      pool_gc_mark (c);

      switch (c->type)
        {
        case TYPE_CONS:
          stack_push (stack, CAR (c));
          stack_push (stack, CDR (c));
          break;

        case TYPE_LAMBDA:
          stack_push (stack, c->lambda.params);
          stack_push (stack, c->lambda.body);
          break;

        case TYPE_INTEGER:
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
gc_mark_state (Stack *stack, State s)
{
  switch (s.state)
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
    gc_mark_reachable (stack, s.u.and_cont.arglist);
    return;
  }

gc_state_and:
  {
    gc_mark_reachable (stack, s.u.and.arglist);
    return;
  }

gc_state_apply:
  {
    gc_mark_reachable (stack, s.u.apply.fn);
    gc_mark_reachable (stack, s.u.apply.arglist);
    return;
  }

gc_state_closure_enter:
  {
    return;
  }

gc_state_closure_leave:
  {
    return;
  }

gc_state_define:
  {
    return;
  }

gc_state_eval_apply:
  {
    gc_mark_reachable (stack, s.u.eval_apply.fn);
    gc_mark_reachable (stack, s.u.eval_apply.arglist);
    return;
  }

gc_state_eval:
  {
    gc_mark_reachable (stack, s.u.eval.expr);
    return;
  }

gc_state_evlis_acc:
  {
    gc_mark_reachable (stack, s.u.evlis_acc.acc);
    gc_mark_reachable (stack, s.u.evlis_acc.arglist);
    return;
  }

gc_state_evlis:
  {
    gc_mark_reachable (stack, s.u.evlis.acc);
    gc_mark_reachable (stack, s.u.evlis.arglist);
    return;
  }

gc_state_funcall:
  {
    gc_mark_reachable (stack, s.u.funcall.fn);
    gc_mark_reachable (stack, s.u.funcall.arglist);
    return;
  }

gc_state_if_:
  {
    gc_mark_reachable (stack, s.u.if_.form);
    return;
  }

gc_state_if_cont:
  {
    gc_mark_reachable (stack, s.u.if_cont.form);
    return;
  }

gc_state_lambda:
  {
    gc_mark_reachable (stack, s.u.lambda.fn);
    gc_mark_reachable (stack, s.u.lambda.arglist);
    return;
  }

gc_state_let:
  {
    gc_mark_reachable (stack, s.u.let.arglist);
    return;
  }

gc_state_lispm:
  {
    gc_mark_reachable (stack, s.u.lispm.fn);
    gc_mark_reachable (stack, s.u.lispm.arglist);
    return;
  }

gc_state_map_acc:
  {
    gc_mark_reachable (stack, s.u.map_acc.fn);
    gc_mark_reachable (stack, s.u.map_acc.acc);
    gc_mark_reachable (stack, s.u.map_acc.ziplist);
    return;
  }

gc_state_map_cont:
  {
    gc_mark_reachable (stack, s.u.map_cont.fn);
    gc_mark_reachable (stack, s.u.map_cont.acc);
    gc_mark_reachable (stack, s.u.map_cont.ziplist);
    return;
  }

gc_state_map:
  {
    gc_mark_reachable (stack, s.u.map.fn);
    gc_mark_reachable (stack, s.u.map.arglist);
    return;
  }

gc_state_or_cont:
  {
    gc_mark_reachable (stack, s.u.or_cont.arglist);
    return;
  }

gc_state_or:
  {
    gc_mark_reachable (stack, s.u.or.arglist);
    return;
  }

gc_state_progn_eval:
  {
    gc_mark_reachable (stack, s.u.progn_eval.arglist);
    return;
  }

gc_state_progn:
  {
    gc_mark_reachable (stack, s.u.progn.res);
    gc_mark_reachable (stack, s.u.progn.arglist);
    return;
  }

gc_state_set:
  {
    return;
  }
}

static void
gc_mark (LM *lm)
{
  Stack *stack = stack_create ();

  for (size_t i = 0; i < lm->stk.sp; ++i)
    gc_mark_reachable (stack, lm->stk.cells[i]);

  for (size_t i = 0; i < lm->env.sp; ++i)
    {
      DictIter iter = dict_iter (lm->env.dict[i]);
      DictEntity *entity;

      while ((entity = dict_items (&iter)))
        gc_mark_reachable (stack, entity->val);
    }

  for (size_t i = 0; i < lm->ctl.sp; ++i)
    gc_mark_state (stack, lm->ctl.states[i]);

  stack_destroy (stack);
}

static void
gc_sweep (Pool *p, void *ptr)
{
  Cell *c = ptr;

  if (!pool_gc_is_free (c) && !pool_gc_is_marked (c))
    pool_free (p, c);
}

void
lm_gc (LM *lm)
{
  gc_mark (lm);
  pool_map_hier (lm->pool, gc_sweep);
}
