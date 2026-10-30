#ifndef XALLOC_H
#define XALLOC_H

#include <stddef.h>
#include <stdlib.h>

#define OOM_STRINGIFY_HELPER(x) #x
#define OOM_STRINGIFY(x)        OOM_STRINGIFY_HELPER (x)
#define OOM_LOCATION            "[" __FILE__ ":" OOM_STRINGIFY (__LINE__) "] "

// Allocation failure is fatal; there is no configurable callback or fake
// pointer return. Marking this noreturn makes that contract explicit.
_Noreturn void xalloc_die (const char *message);

static inline void *
xmalloc (size_t size)
{
  void *pointer = malloc (size);
  if (!pointer)
    xalloc_die (OOM_LOCATION);
  return pointer;
}

static inline void *
xcalloc (size_t count, size_t size)
{
  void *pointer = calloc (count, size);
  if (!pointer)
    xalloc_die (OOM_LOCATION);
  return pointer;
}

static inline void *
xrealloc (void *pointer, size_t size)
{
  pointer = realloc (pointer, size);
  if (!pointer)
    xalloc_die (OOM_LOCATION);
  return pointer;
}

#endif
