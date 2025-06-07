#ifndef ENV_H
#define ENV_H

#include <stdbool.h>
#include <stdlib.h>

struct env;
typedef struct env Env;

Env *env_create (void);
void env_destroy (Env *env);
void env_leave_frame (Env **frame);
bool env_has_key (Env *frame, const char *key);
void *env_lookup (Env *frame, const char *key);
bool env_let (Env *frame, const char *key, void *val);
bool env_set (Env *frame, const char *key, void *val);
void env_enter_frame (Env **frame);
void env_leave_frame (Env **frame);
void env_reset (Env **frame);

#endif
