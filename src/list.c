#include <stdlib.h>

#include "list.h"
#include "safe_str.h"
#include "xalloc.h"

#ifndef LIST_INIT_CAPACITY
#define LIST_INIT_CAPACITY 4
#endif

static bool list_xresize (List *lst, size_t min_capacity);

// thanks python
static bool
list_xresize (List *lst, size_t min_capacity)
{
  size_t new_capacity = lst->capacity;

  while (new_capacity < min_capacity)
    {
      new_capacity += (new_capacity >> 3) + (new_capacity < 9 ? 3 : 6);
    }

  void **ptr = xrealloc (lst->items, new_capacity * sizeof (void *));

  lst->items = ptr;
  lst->capacity = new_capacity;

  return true;
}

List *
list_create (void)
{
  List *lst = xcalloc (1, sizeof (List));

  lst->items = xcalloc (LIST_INIT_CAPACITY, sizeof *(lst->items));
  lst->capacity = LIST_INIT_CAPACITY;
  lst->count = 0;

  return lst;
}

void
list_destroy (List *lst)
{
  free (lst->items);
  free (lst);
}

bool
list_append (List *lst, void *item)
{
  if (lst->count >= lst->capacity)
    list_xresize (lst, lst->count + 1);

  lst->items[lst->count++] = item;
  return true;
}

List *
list_copy (List *lst)
{
  List *new = list_create ();

  for (size_t i = 0; i < lst->count; ++i)
    list_append (new, lst->items[i]);

  return new;
}

void
list_remove_index (List *lst, size_t i)
{
  if (i >= lst->count)
    return;

  for (size_t j = i + 1; j < lst->count; ++j)
    lst->items[j - 1] = lst->items[j];

  lst->count--;
}

void
list_reverse (List *lst)
{
  if (!lst->count)
    return;

  size_t start = 0;
  size_t end = lst->count - 1;

  while (start < end)
    {
      void *tmp = lst->items[start];
      lst->items[start++] = lst->items[end];
      lst->items[end--] = tmp;
    }
}
