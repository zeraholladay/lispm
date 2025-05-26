#include <stdlib.h>

#include "list.h"
#include "safe_str.h"
#include "xalloc.h"

#ifndef HEAP_LIST_INIT_CAPACITY
#define HEAP_LIST_INIT_CAPACITY 4
#endif

// thanks python
static int
list_xresize (List *list, size_t min_capacity)
{
  size_t new_capacity = list->capacity;

  while (new_capacity < min_capacity)
    {
      new_capacity += (new_capacity >> 3) + (new_capacity < 9 ? 3 : 6);
    }

  void **new_nodes = xrealloc (list->items, new_capacity * sizeof (void *));

  list->items = new_nodes;
  list->capacity = new_capacity;

  return 0;
}

List *
list_xalloc (void)
{
  List *list = xcalloc (1, sizeof (List));
  list->items = xcalloc (HEAP_LIST_INIT_CAPACITY, sizeof *(list->items));

  list->capacity = HEAP_LIST_INIT_CAPACITY;
  list->count = 0;

  return list;
}

void
list_free (List *list)
{
  if (!list)
    return;
  free (list->items);
  free (list);
}

int
list_append (List *list, void *item)
{
  if ((list->count >= list->capacity)
      && (list_xresize (list, list->count + 1) != 0))
    return -1;
  list->items[list->count] = item;
  return list->count++; // ie index of append
}

void
list_remove_index (List *list, size_t i)
{
  if (i >= list->count)
    return;

  for (size_t j = i + 1; j < list->count; ++j)
    {
      list->items[j - 1] = list->items[j];
    }

  list->count--;
}
