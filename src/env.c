#include <stdlib.h>

#include "env.h"
#include "oom_handlers.h"
#include "safe_str.h"

#ifndef ENV_STR_MAX
#define ENV_STR_MAX 256
#endif

extern oom_handler_t env_oom_handler;

Env *
env_new (Env *parent)
{
  struct Env *env = calloc (1, sizeof *(env));

  if (!env)
    {
      env_oom_handler (NULL, OOM_LOCATION);
      return NULL;
    }

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

  rb_node *n = rb_alloc (); // FIXME

  if (!n)
    {
      env_oom_handler (NULL, OOM_LOCATION);
      return -1;
    }

  RB_KEY (n) = sym;
  RB_KEY_LEN (n) = len;
  RB_VAL (n) = addr;

  rb_insert (&env->root, n);

  return 0;
}
