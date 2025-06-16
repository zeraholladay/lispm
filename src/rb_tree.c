#include <string.h>

#include "rb_tree.h"
#include "safe_str.h"
#include "stack.h"

#define RB_BLACK        0
#define RB_RED          1
#define RB_LEFT(n)      ((n)->left)
#define RB_RIGHT(n)     ((n)->right)
#define RB_PARENT(n)    ((n)->parent)
#define RB_COLOR(n)     ((n)->color)
#define RB_GET_COLOR(n) ((n) ? (RB_COLOR (n)) : (RB_BLACK))
#define RB_GRANDPARENT(n)                                                     \
  (((n) && (RB_PARENT (n))) ? (RB_PARENT (RB_PARENT (n))) : (NULL))
#define RB_SIBLING(n)                                                         \
  ((n == RB_LEFT (RB_PARENT (n))) ? (RB_RIGHT (RB_PARENT (n)))                \
                                  : (RB_LEFT (RB_PARENT (n))))

inline static void     rb_insert_bal (rb_node **root, rb_node *n);
inline static void     rb_remove_bal (rb_node **root, rb_node *n);
inline static void     rb_rotate_left (rb_node **root, rb_node *n);
inline static void     rb_rotate_right (rb_node **root, rb_node *n);
inline static rb_node *rb_uncle (rb_node *n);

inline static void
rb_insert_bal (rb_node **root, rb_node *n)
{
  rb_node *grandparent, *uncle;

  if (*root == n)
    RB_COLOR (n) = RB_BLACK;
  else if (RB_RED == RB_GET_COLOR (RB_PARENT (n)))
    {
      uncle       = rb_uncle (n);
      grandparent = RB_GRANDPARENT (n);

      if (RB_RED == RB_GET_COLOR (uncle))
        {
          RB_COLOR (RB_PARENT (n)) = RB_COLOR (uncle) = RB_BLACK;
          RB_COLOR (grandparent)                      = RB_RED;
          rb_insert_bal (root, grandparent);
          return;
        }

      if ((n == RB_RIGHT (RB_PARENT (n)))
          && (RB_PARENT (n) == RB_LEFT (grandparent)))
        {
          rb_rotate_left (root, RB_PARENT (n));
          n = RB_LEFT (n);
        }
      else if ((n == RB_LEFT (RB_PARENT (n)))
               && (RB_PARENT (n) == RB_RIGHT (grandparent)))
        {
          rb_rotate_right (root, RB_PARENT (n));
          n = RB_RIGHT (n);
        }

      grandparent              = RB_GRANDPARENT (n);
      RB_COLOR (RB_PARENT (n)) = RB_BLACK;
      RB_COLOR (grandparent)   = RB_RED;

      if ((n == RB_LEFT (RB_PARENT (n)))
          && (RB_PARENT (n) == RB_LEFT (grandparent)))
        rb_rotate_right (root, grandparent);
      else
        rb_rotate_left (root, grandparent);
    }
}

inline static void
rb_remove_bal (rb_node **root, rb_node *n)
{
  rb_node *sibling;

  if (!RB_PARENT (n))
    return;

  sibling = RB_SIBLING (n);

  if (RB_RED == RB_GET_COLOR (sibling))
    {
      RB_COLOR (RB_PARENT (n)) = RB_RED;
      RB_COLOR (sibling)       = RB_BLACK;

      if (n == RB_LEFT (RB_PARENT (n)))
        rb_rotate_left (root, RB_PARENT (n));
      else
        rb_rotate_right (root, RB_PARENT (n));

      sibling = RB_SIBLING (n);
    }

  if (RB_BLACK == RB_GET_COLOR (RB_PARENT (n))
      && RB_BLACK == RB_GET_COLOR (sibling)
      && RB_BLACK == RB_GET_COLOR (RB_LEFT (sibling))
      && RB_BLACK == RB_GET_COLOR (RB_RIGHT (sibling)))
    {
      RB_COLOR (sibling) = RB_RED;
      rb_remove_bal (root, RB_PARENT (n));
      return;
    }

  if (RB_RED == RB_GET_COLOR (RB_PARENT (n))
      && RB_BLACK == RB_GET_COLOR (sibling)
      && RB_BLACK == RB_GET_COLOR (RB_LEFT (sibling))
      && RB_BLACK == RB_GET_COLOR (RB_RIGHT (sibling)))
    {
      RB_COLOR (sibling)       = RB_RED;
      RB_COLOR (RB_PARENT (n)) = RB_BLACK;
    }
  else
    {
      if (RB_LEFT (RB_PARENT (n)) == n
          && RB_BLACK == RB_GET_COLOR (RB_RIGHT (sibling))
          && RB_RED == RB_GET_COLOR (RB_LEFT (sibling)))
        {
          RB_COLOR (sibling)           = RB_RED;
          RB_COLOR (RB_LEFT (sibling)) = RB_BLACK;
          rb_rotate_right (root, sibling);
        }
      else if (RB_RIGHT (RB_PARENT (n)) == n
               && RB_BLACK == RB_GET_COLOR (RB_LEFT (sibling))
               && RB_RED == RB_GET_COLOR (RB_RIGHT (sibling)))
        {
          RB_COLOR (sibling)            = RB_RED;
          RB_COLOR (RB_RIGHT (sibling)) = RB_BLACK;
          rb_rotate_left (root, sibling);
        }

      sibling                  = RB_SIBLING (n);
      RB_COLOR (sibling)       = RB_GET_COLOR (RB_PARENT (n));
      RB_COLOR (RB_PARENT (n)) = RB_BLACK;

      if (RB_LEFT (RB_PARENT (n)) == n)
        {
          RB_COLOR (RB_RIGHT (sibling)) = RB_BLACK;
          rb_rotate_left (root, RB_PARENT (n));
        }
      else
        {
          RB_COLOR (RB_LEFT (sibling)) = RB_BLACK;
          rb_rotate_right (root, RB_PARENT (n));
        }
    }
}

inline static void
rb_rotate_left (rb_node **root, rb_node *n)
{
  rb_node *right = RB_RIGHT (n);

  if ((RB_RIGHT (n) = RB_LEFT (right)))
    RB_PARENT (RB_LEFT (right)) = n;
  if ((RB_PARENT (right) = RB_PARENT (n)))
    {
      if (n == RB_LEFT (RB_PARENT (right)))
        RB_LEFT (RB_PARENT (n)) = right;
      else
        RB_RIGHT (RB_PARENT (n)) = right;
    }
  else
    *root = right;

  RB_LEFT (right) = n;
  RB_PARENT (n)   = right;
}

inline static void
rb_rotate_right (rb_node **root, rb_node *n)
{
  rb_node *left = RB_LEFT (n);

  if ((RB_LEFT (n) = RB_RIGHT (left)))
    RB_PARENT (RB_RIGHT (left)) = n;
  if ((RB_PARENT (left) = RB_PARENT (n)))
    {
      if (n == RB_LEFT (RB_PARENT (left)))
        RB_LEFT (RB_PARENT (left)) = left;
      else
        RB_RIGHT (RB_PARENT (left)) = left;
    }
  else
    *root = left;

  RB_RIGHT (left) = n;
  RB_PARENT (n)   = left;
}

inline static rb_node *
rb_uncle (rb_node *n)
{
  rb_node *grandparent;

  if (!(grandparent = RB_GRANDPARENT (n)))
    return NULL;
  if (RB_PARENT (n) == RB_LEFT (grandparent))
    return RB_RIGHT (grandparent);

  return RB_LEFT (grandparent);
}

void
rb_insert (rb_node **root, rb_node *n)
{
  rb_node *cur, *parent;

  cur = *root;

  while (cur)
    {
      parent = cur;

      int cmp
          = safe_strncmp_minlen (RB_KEY (n), RB_KEY (cur), RB_KEY_LEN (n) + 1);

      if (cmp == 0)
        return;

      cur = (cmp < 0) ? RB_LEFT (cur) : RB_RIGHT (cur);
    }

  RB_PARENT (n) = RB_LEFT (n) = RB_RIGHT (n) = NULL;
  RB_COLOR (n)                               = RB_RED;

  if (NULL == *root)
    *root = n;
  else
    {
      RB_PARENT (n) = parent;
      int cmp       = safe_strncmp_minlen (RB_KEY (n), RB_KEY (parent),
                                           RB_KEY_LEN (n) + 1);

      if (cmp < 0)
        RB_LEFT (parent) = n;
      else
        RB_RIGHT (parent) = n;
    }

  rb_insert_bal (root, n);
}

rb_node *
rb_xalloc (void)
{
  rb_node *n = xcalloc (1, sizeof (*n));
  return n;
}

rb_node *
rb_lookup (rb_node *root, const char *key, size_t key_len)
{
  rb_node *cur = root;

  while (cur)
    {
      int cmp = safe_strncmp_minlen (key, RB_KEY (cur), key_len + 1);

      if (cmp == 0)
        return cur;
      cur = (cmp < 0) ? RB_LEFT (cur) : RB_RIGHT (cur);
    }

  return NULL;
}

rb_node *
rb_remove (rb_node **root, rb_node *n)
{
  rb_node *child, *tmp, phantom = {};

  if (RB_LEFT (n) && RB_RIGHT (n))
    {
      tmp = RB_LEFT (n);

      while (RB_RIGHT (tmp))
        tmp = RB_RIGHT (tmp);

      RB_KEY (n)     = RB_KEY (tmp);
      RB_KEY_LEN (n) = RB_KEY_LEN (tmp);
      RB_VAL (n)     = RB_VAL (tmp);

      n = tmp;
    }

  if (RB_LEFT (n))
    child = RB_LEFT (n);
  else if (RB_RIGHT (n))
    child = RB_RIGHT (n);
  else
    child = &phantom;

  if (!RB_PARENT (n))
    *root = child;
  else
    {
      if (n == RB_LEFT (RB_PARENT (n)))
        RB_LEFT (RB_PARENT (n)) = child;
      else
        RB_RIGHT (RB_PARENT (n)) = child;
    }

  if (child != RB_LEFT (n))
    RB_LEFT (child) = RB_LEFT (n);
  if (child != RB_RIGHT (n))
    RB_RIGHT (child) = RB_RIGHT (n);

  RB_PARENT (child) = RB_PARENT (n);
  RB_LEFT (n) = RB_RIGHT (n) = RB_PARENT (n) = NULL;

  if (RB_BLACK == RB_GET_COLOR (n))
    {
      if (RB_RED == RB_GET_COLOR (child))
        RB_COLOR (child) = RB_BLACK;
      else
        rb_remove_bal (root, child);
    }

  if (&phantom == child)
    {
      if (RB_PARENT (child))
        {
          if (child == RB_LEFT (RB_PARENT (child)))
            RB_LEFT (RB_PARENT (child)) = NULL;
          else
            RB_RIGHT (RB_PARENT (child)) = NULL;
        }
      else if (*root == &phantom)
        *root = NULL;
    }

  return n;
}

void
rb_post_order_iter (rb_node *root, Stack *tmp_stack, Stack *stack)
{
  if (!root)
    return;

  stack_push (tmp_stack, root);

  rb_node *n;

  while ((n = stack_pop (tmp_stack)))
    {
      stack_push (stack, n);

      if (n->left)
        stack_push (tmp_stack, RB_LEFT (n));

      if (n->right)
        stack_push (tmp_stack, RB_RIGHT (n));
    }
}
