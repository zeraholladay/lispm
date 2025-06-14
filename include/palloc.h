#ifndef PALLOC_H
#define PALLOC_H

#include <stdlib.h>

typedef struct Wrapper
{
  struct Wrapper *next_free;
  void *ptr;
} Wrapper;

typedef struct Pool Pool;

struct Pool
{
  Wrapper *free_list, *pool;
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
void pool_map (Pool *p, void (*cb) (Pool *p, void *));
void pool_map_hier (Pool *p, void (*cb) (Pool *p, void *));

#endif
