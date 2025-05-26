#include <stdlib.h>

#include "env.h"
#include "safe_str.h"
#include "xalloc.h"

#ifndef ENV_STR_MAX
#define ENV_STR_MAX 256
#endif

Env *
env_new (Env *parent)
{
  Env *env = xcalloc (1, sizeof *(env));
  env->parent = parent;
  return env;
}

rb_node *
env_lookup (Env *env, const char *sym)
{
  size_t len = safe_strnlen (sym, ENV_STR_MAX);

  for (Env *cur_env = env; cur_env; cur_env = cur_env->parent)
    {
      rb_node *n = rb_lookup (cur_env->root, sym, len);

      if (n)
        return n;
    }

  return NULL;
}

int
env_set (Env *env, const char *sym, void *addr)
{
  size_t len = safe_strnlen (sym, ENV_STR_MAX);

  for (Env *cur_env = env; cur_env; cur_env = cur_env->parent)
    {
      rb_node *n = rb_lookup (cur_env->root, sym, len);

      if (n)
        {
          RB_VAL (n) = addr;
          return 0;
        }
    }

  rb_node *n = rb_xalloc (); // FIXME

  RB_KEY (n) = sym;
  RB_KEY_LEN (n) = len;
  RB_VAL (n) = addr;

  rb_insert (&env->root, n);

  return 0;
}
