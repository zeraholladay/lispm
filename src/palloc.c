#include <stdalign.h>
#include <stddef.h>

#include "palloc.h"
#include "xalloc.h"

#define STRIDE(size)                                                          \
  (((size) + alignof (max_align_t) - 1) & ~(alignof (max_align_t) - 1))

#define INDEX(base, index, stride)                                            \
  ((PallocWrapper *)((char *)(base) + ((index) * (stride))))

Pool *
pool_init (size_t count, size_t size)
{
  Pool *p = xcalloc (1, sizeof *(p));
  size_t stride = STRIDE (sizeof (PallocWrapper)
                          + size); // array-aligned PallocWrapper and size

  p->pool = xcalloc (count, stride);

  p->count = count;
  p->stride = stride;
  p->size = size;
  p->next = p->prev = NULL;

  pool_reset_all (p);

  return p;
}

void
pool_destroy (Pool **p)
{
  free ((*p)->pool), (*p)->pool = NULL;
  free (*p), *p = NULL;
}

void
pool_destroy_hier (Pool **head)
{
  Pool *next;

  for (Pool *cur = *head; cur; cur = next)
    {
      next = cur->next;
      pool_destroy (&cur);
    }

  *head = NULL;
}

void *
pool_xalloc (Pool *p)
{
  if (!p->free_list)
    return oom_handler_die (p, OOM_LOCATION "free_list empty");

  PallocWrapper *wrapper = p->free_list;
  p->free_list = wrapper->next_free;

  wrapper->free = 0;
  wrapper->gc_mark = 0;

  return &wrapper->ptr;
}

void *
pool_xalloc_hier (Pool **head)
{
  Pool *cur = *head;

  if (cur->free_list)
    return pool_xalloc (cur);

  Pool *new_pool = pool_init (cur->count, cur->size);

  new_pool->prev = NULL;
  new_pool->next = *head;
  cur->prev = new_pool;
  *head = new_pool;

  return pool_xalloc (new_pool);
}

void
pool_free (Pool *p, void *ptr)
{
  PallocWrapper *wrapper
      = (PallocWrapper *)((void *)ptr - offsetof (PallocWrapper, ptr));

  if (wrapper->free)
    return;

  wrapper->gc_mark = 0;
  wrapper->free = 1;
  wrapper->next_free = p->free_list;
  p->free_list = wrapper;
}

void
pool_reset_all (Pool *p)
{
  size_t count = p->count;
  size_t stride = p->stride;

  for (size_t i = 0; i < count - 1; ++i)
    {
      PallocWrapper *cur = INDEX (p->pool, i, stride);
      cur->next_free = INDEX (p->pool, i + 1, stride);
    }

  INDEX (p->pool, count - 1, stride)->next_free = NULL;
  p->free_list = INDEX (p->pool, 0, stride);
}

bool
pool_gc_is_marked (void *ptr)
{
  PallocWrapper *wrapper
      = (PallocWrapper *)((void *)ptr - offsetof (PallocWrapper, ptr));
  return wrapper->gc_mark;
}

void
pool_gc_mark (void *ptr)
{
  PallocWrapper *wrapper
      = (PallocWrapper *)((void *)ptr - offsetof (PallocWrapper, ptr));
  wrapper->gc_mark = 1;
}

void
pool_map (Pool *p, void (*cb) (Pool *p, void *))
{
  for (size_t i = 0; i < p->count - 1; ++i)
    {
      PallocWrapper *wrapper = INDEX (p->pool, i, p->stride);
      cb (p, &wrapper->ptr);
    }
}

void
pool_map_hier (Pool *head, void (*cb) (Pool *p, void *))
{
  Pool *cur_p = head;

  while (cur_p)
    {
      pool_map (cur_p, cb);
      cur_p = cur_p->next;
    }
}
