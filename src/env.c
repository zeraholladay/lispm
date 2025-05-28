#include <stdlib.h>

#include "dict.h"
#include "env.h"
#include "xalloc.h"

#ifndef ENV_STR_MAX
#define ENV_STR_MAX 256
#endif

struct env
{
  Dict *dict;
  Env *parent;
};

Env *
env_create (void)
{
  Env *env = xmalloc (sizeof *(env));
  env->dict = DICT_CREATE ();
  env->parent = NULL;
  return env;
}

void
env_destroy (Env *env)
{
  dict_destroy (env->dict);
  free (env);
}

bool
env_has_key (Env *frame, const char *key)
{
  return dict_has_key (frame->dict, key);
}

void *
env_lookup (Env *frame, const char *key)
{
  while (frame)
    {
      DictEntity *entity = dict_lookup (frame->dict, key);
      if (entity)
        return entity->val;
      frame = frame->parent;
    }
  return NULL;
}

bool
env_let (Env *frame, const char *key, void *val)
{
  return dict_insert (frame->dict, key, val);
}

bool
env_set (Env *frame, const char *key, void *val)
{
  Env *cur = frame;

  while (cur->parent)
    cur = cur->parent;

  return dict_insert (cur->dict, key, val);
}

void
env_enter_frame (Env **frame)
{
  if (!frame || !*frame)
    return;
  Env *chld = env_create ();
  chld->parent = *frame;
  *frame = chld;
}

void
env_leave_frame (Env **frame)
{
  if (!frame || !*frame || !(*frame)->parent)
    return;
  Env *chld_frame = *frame;
  *frame = (*frame)->parent;
  env_destroy (chld_frame);
}