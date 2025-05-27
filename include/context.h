#ifndef CONTEXT_H
#define CONTEXT_H

#include "env.h"
#include "palloc.h"
#include "rb_tree.h"

#define CTX_POOL(ctx) ((ctx)->node_pool)
#define CTX_SYMTAB(ctx) ((ctx)->parser_ctx.sym_tab)
#define CTX_PARSE_ROOT(ctx) ((ctx)->parser_ctx.root_node)

struct Node;

typedef struct ParserContext
{
  rb_node *sym_tab;
  struct Node *root_node;
} ParserContext;

typedef struct Context
{
  Pool *node_pool;
  Env *env;
  ParserContext parser_ctx;
} Context;

#endif
