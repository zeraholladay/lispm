#include <stdlib.h>

#include "list.h"
#include "safe_str.h"
#include "xalloc.h"

static bool list_xresize (List *list, size_t min_capacity);

// thanks python
static bool
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

  return true;
}

List *
list_create (void)
{
  List *list = xcalloc (1, sizeof (List));

  list->items = xcalloc (HEAP_LIST_INIT_CAPACITY, sizeof *(list->items));
  list->capacity = HEAP_LIST_INIT_CAPACITY;
  list->count = 0;

  return list;
}

void
list_destroy (List *list)
{
  free (list->items);
  free (list);
}

bool
list_append (List *list, void *item)
{
  if (list->count >= list->capacity)
    list_xresize (list, list->count + 1);

  list->items[list->count++] = item;
  return true;
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
