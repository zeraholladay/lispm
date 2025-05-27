#ifndef LIST_H
#define LIST_H

#include <stdbool.h>
#include <stdlib.h>

#ifndef HEAP_LIST_INIT_CAPACITY
#define HEAP_LIST_INIT_CAPACITY 4
#endif

typedef struct
{
  size_t count;
  size_t capacity;
  void **items;
} List;

List *list_create (void);
void list_destroy (List *list);
bool list_append (List *list, void *item);
void list_remove_index (List *list, size_t i);

#endif
