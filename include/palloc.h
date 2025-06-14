#ifndef PALLOC_H
#define PALLOC_H

#include <stdbool.h>
#include <stdlib.h>

#define PALLOC_WRAPPER_FIELDS(_type)                                          \
  unsigned free : 1, gc_mark : 1;                                             \
  _type *next_free;

typedef struct palloc_wrapper PallocWrapper;

struct palloc_wrapper
{
  PALLOC_WRAPPER_FIELDS (PallocWrapper)
  void *ptr;
};

typedef struct pool Pool;

struct pool
{
  PallocWrapper *free_list, *pool;
  size_t count, stride, size;
  Pool *prev, *next;
};

Pool *pool_init (size_t count, size_t size);
void pool_destroy (Pool **p);
void pool_destroy_hier (Pool **head);
void *pool_xalloc (Pool *p);
void *pool_xalloc_hier (Pool **head);
void pool_free (Pool *p, void *ptr);
void pool_reset_all (Pool *p);
bool pool_gc_is_free (void *ptr);
bool pool_gc_is_marked (void *ptr);
void pool_gc_mark (void *ptr);
void pool_gc_unmark (void *ptr);
void pool_map (Pool *p, void (*cb) (Pool *p, void *));
void pool_map_hier (Pool *head, void (*cb) (Pool *p, void *));

#endif
