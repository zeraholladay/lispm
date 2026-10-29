#include <string.h>

#include "dict.h"
#include "safe_str.h"

#include "xalloc.h"

#define EMPTY     -1
#define TOMBSTONE -2

#define HASH(str)          ((size_t)djb2 (str))
#define LIST_IDX(d_ptr, i) ((DictEntity *)((d_ptr)->list->items[i]))
#define STR_EQ(entity, str, str_len)                                          \
  ((entity)->len == (str_len) && memcmp ((entity)->key, (str), (str_len)) == 0)

static size_t
get_bins_idx (Dict *dict, size_t hash_key, const char *key, size_t len)
{
  size_t  start_bin_idx, bin_idx;
  ssize_t first_tombstone = -1;
  int    *bins            = dict->bins, bin_val;

  // ie hash_key & 0x0111 when dict->capacity is 8
  start_bin_idx = bin_idx = hash_key & (dict->capacity - 1);

  do
    {
      bin_val = bins[bin_idx];

      if (bin_val == EMPTY)
        return (first_tombstone >= 0) ? (size_t)first_tombstone : bin_idx;
      else if (bin_val == TOMBSTONE)
        {
          if (first_tombstone == -1)
            first_tombstone = bin_idx;
        }
      else if (STR_EQ (LIST_IDX (dict, bin_val), key, len))
        return bin_idx;

      bin_idx = (bin_idx + 1) & (dict->capacity - 1);
    }
  while (bin_idx != start_bin_idx);

  return first_tombstone >= 0 ? (size_t)first_tombstone : dict->capacity;
}

static int *
xalloc_bins (size_t capacity)
{
  int *bins = xmalloc (capacity * sizeof *(bins));

  for (size_t i = 0; i < capacity; ++i)
    bins[i] = EMPTY;

  return bins;
}

static int
xgrow_bins (Dict *dict)
{
  int   *old_bins, old_bin_val;
  size_t new_capacity = dict->capacity * 2;
  size_t old_capacity;

  int *new_bins = xalloc_bins (new_capacity);

  old_capacity   = dict->capacity;
  dict->capacity = new_capacity;

  old_bins   = dict->bins;
  dict->bins = new_bins;

  // reindex
  for (size_t i = 0; i < old_capacity; ++i)
    {
      old_bin_val = old_bins[i];

      if (old_bin_val == EMPTY || old_bin_val == TOMBSTONE)
        continue;

      DictEntity *entity = LIST_IDX (dict, old_bin_val);

      size_t      hash_key = entity->hash_key;
      const char *key      = entity->key;
      size_t      len      = entity->len;

      size_t new_bins_idx = get_bins_idx (dict, hash_key, key, len);

      new_bins[new_bins_idx] = old_bin_val;
    }

  free (old_bins);
  return 0;
}

Dict *
dict_create_va_list (const char *key, ...)
{
  Dict   *dict = dict_create (NULL, 0);
  va_list ap;

  va_start (ap, key);
  for (const char *k = key; k; k = va_arg (ap, const char *))
    dict_insert (dict, k, va_arg (ap, void *));
  va_end (ap);

  return dict;
}

Dict *
dict_create (const DictEntity *entities, size_t n)
{
  Dict *dict = xmalloc (sizeof *(dict));

  dict->count    = 0;
  dict->capacity = DICT_INIT_CAP;

  dict->bins = xalloc_bins (dict->capacity);
  dict->list = list_create ();

  for (size_t i = 0; i < n; i++)
    {
      bool res = dict_insert (dict, entities[i].key, entities[i].val);

      if (!res)
        return NULL;
    }

  return dict;
}

void
dict_destroy (Dict *dict)
{
  List *list = dict->list;

  for (size_t i = 0; i < list->count; ++i)
    free (list->items[i]);

  list_destroy (list);
  free (dict->bins);
  free (dict);
}

void
dict_del (Dict *dict, const char *key)
{
  size_t len      = strlen (key);
  size_t bins_idx = get_bins_idx (dict, HASH (key), key, len);
  if (bins_idx == dict->capacity)
    return;
  int bin_val = dict->bins[bins_idx];

  if (bin_val >= 0)
    {
      free (LIST_IDX (dict, bin_val));
      dict->bins[bins_idx] = TOMBSTONE;
      size_t last          = --dict->list->count;
      if ((size_t)bin_val != last)
        {
          DictEntity *moved = LIST_IDX (dict, last);
          size_t      moved_bin
              = get_bins_idx (dict, moved->hash_key, moved->key, moved->len);
          dict->list->items[bin_val] = moved;
          dict->bins[moved_bin]      = bin_val;
        }
      dict->list->items[last] = NULL;
      --dict->count;
    }
}

bool
dict_has_key (Dict *dict, const char *key)
{
  return dict_lookup (dict, key) != NULL;
}

bool
dict_insert (Dict *dict, const char *key, void *val)
{
  if ((dict->count + 1) * 5 > dict->capacity * 4) // 80% >
    xgrow_bins (dict);

  size_t hash_key = HASH (key);
  size_t len      = strlen (key);
  size_t bin_idx  = get_bins_idx (dict, hash_key, key, len);

  if (bin_idx == dict->capacity)
    {
      xgrow_bins (dict);
      bin_idx = get_bins_idx (dict, hash_key, key, len);
    }

  int bin_val = dict->bins[bin_idx];

  if (bin_val >= 0)
    {
      LIST_IDX (dict, bin_val)->val = val;
      return dict->count;
    }

  DictEntity *entity = xmalloc (sizeof *(entity));

  entity->hash_key = hash_key;
  entity->key      = key;
  entity->len      = len;
  entity->val      = val;

  list_append (dict->list, entity);

  dict->bins[bin_idx] = dict->list->count - 1;
  ++dict->count;

  return true;
}

DictEntity *
dict_lookup (Dict *dict, const char *key)
{
  size_t len      = strlen (key);
  size_t bins_idx = get_bins_idx (dict, HASH (key), key, len);
  if (bins_idx == dict->capacity)
    return NULL;
  int bin_val = dict->bins[bins_idx];

  if (bin_val >= 0)
    return LIST_IDX (dict, bin_val);

  return NULL;
}

DictEntity *
dict_items (DictIter *iter)
{
  Dict *dict = iter->dict;

  while (iter->i < dict->capacity)
    {
      int bin_val = dict->bins[iter->i++];

      if (bin_val == EMPTY || bin_val == TOMBSTONE)
        continue;

      return LIST_IDX (dict, bin_val);
    }

  return NULL;
}
