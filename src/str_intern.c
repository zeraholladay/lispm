#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "palloc.h"
#include "safe_str.h"
#include "str_intern.h"

typedef struct BumpPool
{
  size_t           capacity, offset;
  struct BumpPool *next;
  char             buffer[];
} BumpPool;

static rb_node  *root        = NULL;
static bool      str_internd = false;
static Pool     *pool        = NULL;
static BumpPool *bump_pool   = NULL;

static BumpPool *
xalloc_bump_pool (size_t capacity)
{
  BumpPool *new = xcalloc (1, sizeof (*new) + capacity);
  new->capacity = capacity;
  return new;
}

static char *
bump_pool_xalloc (size_t n)
{
  if (bump_pool->offset + n > bump_pool->capacity)
    {
      size_t    capacity = n > STR_INTERN_BUMP_SIZE ? n : STR_INTERN_BUMP_SIZE;
      BumpPool *new      = xalloc_bump_pool (capacity);
      new->next          = bump_pool;
      bump_pool          = new;
    }
  char *ptr = &bump_pool->buffer[bump_pool->offset];
  bump_pool->offset += n;
  return ptr;
}

static char *
bump_pool_strndup (const char *s, size_t len)
{
  char *new = bump_pool_xalloc (len + 1);
  memcpy (new, s, len);
  new[len] = '\0';
  return new;
}

static void
str_intern_init (void)
{
  static bool cleanup_registered = false;
  if (!cleanup_registered)
    {
      atexit (str_intern_destroy);
      cleanup_registered = true;
    }
  bump_pool = xalloc_bump_pool (STR_INTERN_BUMP_SIZE);
  pool      = pool_init (STR_INTERN_POOL_CAPACITY, sizeof (rb_node));
}

const char *
str_intern (const char *s, size_t len)
{
  if (!str_internd)
    {
      str_internd = true;
      str_intern_init ();
    }

  rb_node *node = rb_lookup (root, s, len);

  if (node)
    return RB_KEY (node);

  node = pool_xalloc_hier (&pool);

  RB_KEY (node)     = bump_pool_strndup (s, len);
  RB_KEY_LEN (node) = len;
  // note: no RB_VAL here. ie symbols don't have values.
  RB_VAL (node) = NULL;

  if (!RB_KEY (node))
    return NULL;

  rb_insert (&root, node);

  return RB_KEY (node);
}

void
str_intern_destroy (void)
{
  while (bump_pool)
    {
      BumpPool *next = bump_pool->next;
      free (bump_pool);
      bump_pool = next;
    }

  pool_destroy_hier (&pool);
  root        = NULL;
  str_internd = false;
}
