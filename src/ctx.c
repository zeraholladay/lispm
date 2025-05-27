#include "ctx.h"
#include "dict.h"
#include "eval.h"
#include "palloc.h"
#include "rb_tree.h"
#include "sym_save.h"
#include "types.h"
#include "xalloc.h"

typedef struct ctxenv_t CtxEnv;

struct ctxenv_t
{
  Dict *dict;
  CtxEnv *parent;
};

struct ctx
{
  rb_node *symtab;
  Pool *pool;
  CtxEnv *env;
};

static bool sym_saved = false;

static CtxEnv *
env_create (CtxEnv *parent)
{
  CtxEnv *env = xcalloc (1, sizeof *(env));
  env->dict = dict_create (NULL, 0);
  env->parent = parent;
  return env;
}

static CtxEnv *
env_destroy (CtxEnv *env)
{
  dict_destroy (env->dict);
  free (env);
}

Ctx *
ctx_create (void)
{
  if (!sym_saved)
    {
      sym_saved = true;
      sym_save_init ();
    }

  Ctx *c = xcalloc (1, sizeof *c);

  c->symtab = NULL;

  c->pool = pool_init (OBJ_POOL_CAPACITY, sizeof (Node));
  c->env = env_create (NULL);

  return c;
}

void
ctx_destroy (Ctx *c)
{
  CtxEnv *frame = c->env;
  while (frame)
    {
      CtxEnv *parent = frame->parent;
      dict_destroy (frame->dict);
      frame = parent;
    }
  pool_destroy (&c->pool);
  free (c);
}

Node *
ctx_create_node (Ctx *c)
{
  return pool_xalloc_hier (&c->pool);
}

void
ctx_env_enter_frame (Ctx *c)
{
  CtxEnv *new_frame = env_create (c->env);
  new_frame->parent = c->env;
  c->env = new_frame;
}

void
ctx_env_exit_frame (Ctx *c)
{
  CtxEnv *cur = c->env;
  c->env = c->env->parent;
  env_destroy (cur);
}

Node *
ctx_env_lookup (Ctx *c, const char *key)
{
  CtxEnv *frame = c->env;
  while (frame)
    {
      DictEntity *entity = dict_lookup (frame->dict, key);
      if (entity)
        return entity->val;
      frame = frame->parent;
    }
  return NIL;
}

bool
ctx_env_set (Ctx *c, const char *key, Node *n)
{
  return dict_insert (c->env->dict, key, n);
}

const char *
ctx_intern_str (Ctx *c, const char *str, size_t len)
{
  return sym_save (&c->symtab, str, len);
}
