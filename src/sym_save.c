#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "palloc.h"
#include "safe_str.h"
#include "sym_save.h"

typedef struct BumpPool
{
  size_t offset;
  struct BumpPool *next;
  char buffer[SYM_SAVE_BUMP_SIZE];
} BumpPool;

static Pool *pool = NULL;
static BumpPool *bump_pool = NULL;

static BumpPool *
xalloc_bump_pool (void)
{
  BumpPool *new = xcalloc (1, sizeof *(bump_pool));
  new->offset = 0;
  new->next = NULL;
  return new;
}

static char *
bump_pool_xalloc (size_t n)
{
  if (bump_pool->offset + n > SYM_SAVE_BUMP_SIZE)
    { // FIXME: large symbols
      BumpPool *new = xalloc_bump_pool ();
      new->next = bump_pool;
      bump_pool = new;
    }
  char *ptr = &bump_pool->buffer[bump_pool->offset];
  bump_pool->offset += n;
  return ptr;
}

static char *
bump_pool_strndup (const char *s, size_t len)
{
  len += 1;
  char *new = bump_pool_xalloc (len);
  new[len] = '\0';
  return memcpy (new, s, len);
}

void
sym_save_init (void)
{
  bump_pool = xalloc_bump_pool ();
  pool = pool_init (SYMTAB_POOL_CAPACITY, sizeof (rb_node));
}

// TODO: max symbol size/limit
const char *
sym_save (rb_node **root, const char *s, size_t len)
{
  rb_node *node = rb_lookup (*root, s, len);

  if (node)
    return RB_KEY (node);

  node = pool_xalloc (pool);

  RB_KEY (node) = bump_pool_strndup (s, len);
  RB_KEY_LEN (node) = len;
  // note: no RB_VAL here. ie symbols don't have values.
  RB_VAL (node) = NULL;

  if (!RB_KEY (node))
    return NULL;

  rb_insert (root, node);

  return RB_KEY (node);
}
