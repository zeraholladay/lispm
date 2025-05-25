#include <stdalign.h>
#include <stddef.h>

#include "oom_handlers.h"
#include "palloc.h"

#define STRIDE(size)                                                          \
  (((size) + alignof (max_align_t) - 1) & ~(alignof (max_align_t) - 1))

#define INDEX(base, index, stride)                                            \
  ((Wrapper *)((char *)(base) + ((index) * (stride))))

extern oom_handler_t palloc_oom_handler;

Pool *
pool_init (size_t count, size_t size)
{
  Pool *p = calloc (1, sizeof *(p));
  size_t stride
      = STRIDE (sizeof (Wrapper) + size); // array-aligned Wrapper and size

  if (!p)
    {
      palloc_oom_handler (NULL, OOM_LOCATION);
      return NULL;
    }

  p->pool = calloc (count, stride);

  if (!p->pool)
    {
      free (p), p = NULL;
      palloc_oom_handler (NULL, OOM_LOCATION);
      return NULL;
    }

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
pool_alloc (Pool *p)
{
  if (!p->free_list)
    {
      palloc_oom_handler (p, OOM_LOCATION "free_list empty");
      return NULL;
    }

  Wrapper *wrapper = p->free_list;
  p->free_list = wrapper->next_free;

  return &wrapper->ptr;
}

void *
pool_alloc_hier (Pool **head)
{
  Pool *cur = *head;

  if (cur->free_list)
    {
      return pool_alloc (cur);
    }

  Pool *new_pool = pool_init (cur->count, cur->size);

  if (!new_pool)
    {
      palloc_oom_handler (NULL, OOM_LOCATION);
      return NULL;
    }

  new_pool->prev = NULL;
  new_pool->next = *head;
  cur->prev = new_pool;
  *head = new_pool;

  return pool_alloc (new_pool);
}

void
pool_free (Pool *p, void *ptr)
{
  Wrapper *wrapper = (Wrapper *)((void *)ptr - offsetof (Wrapper, ptr));
  wrapper->next_free = p->free_list;
  p->free_list = wrapper;
}

void
pool_reset_all (Pool *p)
{
  size_t count = p->count;
  size_t stride = p->stride;

  Wrapper *cur;

  for (size_t i = 0; i < count - 1; ++i)
    {
      cur = INDEX (p->pool, i, stride);
      cur->next_free = INDEX (p->pool, i + 1, stride);
    }

  INDEX (p->pool, count - 1, stride)->next_free = NULL;
  p->free_list = INDEX (p->pool, 0, stride);
}
