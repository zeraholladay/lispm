#ifndef EVAL_H
#define EVAL_H

#include "eval_ctx.h"
#include "keywords.h"
#include "types.h"

#define NIL (KEYWORD (NIL))
#define T (KEYWORD (T))

#define IS_NIL(node) (node == NIL)
#define LISTP(node) (IS_NIL (node) || IS_LIST (node))

#define FIRST(node) ((node)->as.list.first)
#define REST(node) ((node)->as.list.rest)

#define CONS(first, rest, ctx) (cons_list (&CTX_POOL (ctx), first, rest))
#define LIST1(item, ctx) (CONS (item, NIL, ctx))
#define LIST2(first, rest, ctx) (CONS (first, LIST1 (rest, ctx), ctx))

#define RPLACD(node, val)                                                     \
  do                                                                          \
    {                                                                         \
      if (IS_LIST (node))                                                     \
        REST (node) = val;                                                    \
    }                                                                         \
  while (0)

Node *eval_append (Node *args, Context *ctx);
Node *eval_apply (Node *args, Context *ctx);
Node *eval_cons (Node *args, Context *ctx);
Node *eval_first (Node *args, Context *ctx);
Node *eval_funcall (Node *args, Context *ctx);
Node *eval_if (Node *expr, Context *ctx);
Node *eval_lambda (Node *expr, Context *ctx);
Node *eval_len (Node *args, Context *ctx);
Node *eval_list (Node *args, Context *ctx);
Node *eval_pair (Node *args, Context *ctx);
Node *eval_print (Node *args, Context *ctx);
Node *eval_reverse (Node *args, Context *ctx);
Node *eval_rest (Node *args, Context *ctx);
Node *eval_set (Node *args, Context *ctx);
Node *eval_str (Node *args, Context *ctx);
Node *eval (Node *form, Context *ctx);
Node *eval_list (Node *args, Context *ctx);
Node *eval_progn (Node *program, Context *ctx);

#endif
