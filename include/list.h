#ifndef LIST_H
#define LIST_H

#include <stdbool.h>
#include <stdlib.h>

typedef struct
{
  size_t count;
  size_t capacity;
  void **items;
} List;

List *list_create (void);
void  list_destroy (List *lst);
bool  list_append (List *lst, void *item);
List *list_copy (List *lst);
void  list_remove_index (List *lst, size_t i);
void  list_reverse (List *lst);

#endif
