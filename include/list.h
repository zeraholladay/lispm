#ifndef LIST_H
#define LIST_H

#include <stdlib.h>

typedef struct
{
  size_t count;
  size_t capacity;
  void **items;
} List;

List *list_xalloc (void);
void list_free (List *list);
int list_append (List *list, void *item);
void list_remove_index (List *list, size_t i);

#endif
