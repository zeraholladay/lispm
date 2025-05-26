#ifndef RB_TREE_H
#define RB_TREE_H

#include "stack.h"
#include <stdlib.h>

#define RB_KEY(n) ((n)->key)
#define RB_KEY_LEN(n) ((n)->key_len)
#define RB_VAL(n) ((n)->val)

typedef struct rb_node
{
  struct rb_node *left, *right, *parent;
  size_t color, key_len;
  const char *key;
  void *val;
} rb_node;

rb_node *rb_xalloc (void);
void rb_insert (rb_node **root, rb_node *n);
rb_node *rb_lookup (rb_node *root, const char *key, size_t key_len);
rb_node *rb_remove (rb_node **root, rb_node *n);
void rb_post_order_iter (rb_node *root, Stack *tmp_stack, Stack *stack);

#endif
