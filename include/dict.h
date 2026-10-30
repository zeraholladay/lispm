#ifndef DICT_H
#define DICT_H

#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>

#include "list.h"

#ifndef DICT_INIT_CAP
#define DICT_INIT_CAP 8
#endif

#define DICT_CREATE()     (dict_create (NULL, 0))
#define DICT_ENTITY(k, v) { .hash_key = 0, .key = k, .len = 0, .val = v }

typedef struct
{
  size_t      hash_key;
  const char *key;
  size_t      len;
  void       *val;
} DictEntity;

typedef struct
{
  size_t count, capacity;
  List  *list;
  int   *bins;
} Dict;

typedef struct
{
  Dict  *dict;
  size_t i;
} DictIter;

static inline DictIter
dict_iter (Dict *d)
{
  return (DictIter){ .dict = d, .i = 0 };
}

Dict       *dict_create_va_list (const char *key, ...);
Dict       *dict_create (const DictEntity *entity, size_t n);
void        dict_destroy (Dict *dict);
void        dict_del (Dict *dict, const char *key);
bool        dict_has_key (Dict *dict, const char *key);
bool        dict_insert (Dict *dict, const char *key, void *val);
DictEntity *dict_lookup (Dict *dict, const char *key);
// Byte-length variants accept non-NUL-terminated slices. Keys are borrowed:
// insertion requires their storage to remain valid until deletion/destruction.
bool        dict_insert_n (Dict *dict, const char *key, size_t len, void *val);
DictEntity *dict_lookup_n (Dict *dict, const char *key, size_t len);
DictEntity *dict_items (DictIter *iter);

#endif
