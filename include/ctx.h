#ifndef CTX_H
#define CTX_H

#include <stdbool.h>
#include <stdlib.h>

#ifndef OBJ_POOL_CAPACITY
#define OBJ_POOL_CAPACITY 4096
#endif

struct Node;
typedef struct Node Node;

typedef struct ctx Ctx;

Ctx *ctx_create (void);
void ctx_destroy (Ctx *c);
Node *ctx_create_node (Ctx *c);
void ctx_env_enter_frame (Ctx *c);
void ctx_env_exit_frame (Ctx *c);
Node *ctx_env_lookup (Ctx *c, const char *key);
bool ctx_env_set (Ctx *c, const char *key, Node *n);
const char *ctx_intern_str (Ctx *c, const char *str, size_t len);

#endif
