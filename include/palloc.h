#ifndef PALLOC_H
#define PALLOC_H

#include <stdlib.h>

typedef struct Wrapper
{
  struct Wrapper *next_free;
  void *ptr;
} Wrapper;

typedef struct Pool
{
  Wrapper *free_list, *pool;
  size_t count, stride, size;
  struct Pool *prev, *next;
} Pool;

Pool *pool_init (size_t count, size_t size);
void pool_destroy (Pool **p);
void pool_destroy_hier (Pool **head);
void *pool_xalloc (Pool *p);
void *pool_xalloc_hier (Pool **head);
void pool_free (Pool *p, void *ptr);
void pool_reset_all (Pool *p);

#endif
