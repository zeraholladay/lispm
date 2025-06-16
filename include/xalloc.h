#ifndef XMALLOC_H
#define XMALLOC_H

#include <stddef.h>
#include <stdlib.h>

#ifndef SCRATCH_STACK_LIMIT
#define SCRATCH_STACK_LIMIT 4096
#endif

#define OOM_STRINGIFY_HELPER(x) #x
#define OOM_STRINGIFY(x)        OOM_STRINGIFY_HELPER (x)
#define OOM_LOCATION            "[" __FILE__ ":" OOM_STRINGIFY (__LINE__) "] "

typedef void *(*oom_handler_t) (void *, const char *msg);
void *oom_handler_die (void *void_ptr, const char *msg);

static inline void *
xmalloc (size_t size)
{
  void *ptr = malloc (size);
  if (!ptr)
    return oom_handler_die (NULL, OOM_LOCATION);
  return ptr;
}

static inline void *
xcalloc (size_t count, size_t size)
{
  void *ptr = calloc (count, size);
  if (!ptr)
    return oom_handler_die (NULL, OOM_LOCATION);
  return ptr;
}

static inline void *
xrealloc (void *ptr, size_t size)
{
  ptr = realloc (ptr, size);
  if (!ptr)
    return oom_handler_die (NULL, OOM_LOCATION);
  return ptr;
}

typedef struct
{
  size_t capacity;
  void  *heap;
  char   stack[SCRATCH_STACK_LIMIT];
} scratch_t;

static inline void *
xalloc_scratch (scratch_t *s, size_t nbytes)
{
  if (nbytes <= SCRATCH_STACK_LIMIT)
    {
      s->capacity = SCRATCH_STACK_LIMIT;
      s->heap     = NULL;
      return s->stack;
    }
  else
    {
      s->capacity = nbytes;
      s->heap     = xmalloc (nbytes);
      return s->heap;
    }
}

static inline void
xfree_scratch (scratch_t *s)
{
  if (s->heap)
    {
      free (s->heap);
      s->heap = NULL;
    }
}

#endif
