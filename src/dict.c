#include "dict.h"
#include "safe_str.h"

#define EMPTY -1
#define TOMBSTONE -2

#define HASH(str) ((size_t)djb2 (key))
#define LIST_IDX(d_ptr, i) ((DictEntity *)((d_ptr)->list->items[i]))
#define STR_EQ(s1, s2, len) (!safe_strncmp_minlen (s1, s2, len + 1))

static size_t
get_bins_idx (Dict *dict, size_t hash_key, const char *key, size_t len)
{
  size_t start_bin_idx, bin_idx;
  ssize_t first_tombstone = -1;
  int *bins = dict->bins, bin_val;

  // ie hash_key & 0x0111 when dict->capacity is 8
  start_bin_idx = bin_idx = hash_key & (dict->capacity - 1);

  do
    {
      bin_val = bins[bin_idx];

      if (bin_val == EMPTY)
        {
          return (first_tombstone >= 0) ? (size_t)first_tombstone : bin_idx;
        }
      else if (bin_val == TOMBSTONE)
        {
          if (first_tombstone == -1)
            first_tombstone = bin_idx;
        }
      else if (STR_EQ (key, LIST_IDX (dict, bin_val)->key, len))
        {
          return bin_idx;
        }
      bin_idx = (bin_idx + 1) & (dict->capacity - 1);
    }
  while (bin_idx != start_bin_idx);

  // TODO & FIXME: all bins are occupied or all bins are tombstones: resize
  return first_tombstone >= 0 ? (size_t)first_tombstone : start_bin_idx;
}

static int *
alloc_bins (size_t capacity)
{
  int *bins = malloc (capacity * sizeof *(bins));
  if (!bins)
    {
      return NULL;
    }

  for (size_t i = 0; i < capacity; ++i)
    bins[i] = EMPTY;

  return bins;
}

static int
grow_bins (Dict *dict)
{
  int *old_bins, old_bin_val;
  size_t new_capacity = dict->capacity * 2;
  size_t old_capacity;

  int *new_bins = alloc_bins (new_capacity);
  if (!new_bins)
    {
      return -1;
    }

  old_capacity = dict->capacity;
  dict->capacity = new_capacity;

  old_bins = dict->bins;
  dict->bins = new_bins;

  // reindex
  for (size_t i = 0; i < old_capacity; ++i)
    {
      old_bin_val = old_bins[i];

      if (old_bin_val == EMPTY || old_bin_val == TOMBSTONE)
        {
          continue;
        }

      DictEntity *entity = LIST_IDX (dict, old_bin_val);

      size_t hash_key = entity->hash_key;
      const char *key = entity->key;
      size_t len = entity->len;

      size_t new_bins_idx = get_bins_idx (dict, hash_key, key, len);

      new_bins[new_bins_idx] = old_bin_val;
    }

  free (old_bins);
  return 0;
}

Dict *
dict_alloc_va_list (const char *key, ...)
{
  Dict *dict = dict_alloc (NULL, 0);
  va_list ap;

  va_start (ap, key);
  for (const char *k = key; k; k = va_arg (ap, const char *))
    {
      dict_insert (dict, k, va_arg (ap, void *));
    }
  va_end (ap);

  return dict;
}

Dict *
dict_alloc (const DictEntity *entities, size_t n)
{
  Dict *dict = malloc (sizeof *(dict));
  if (!dict)
    {
      return NULL;
    }

  dict->count = 0;
  dict->capacity = DICT_INIT_CAP;

  dict->bins = alloc_bins (dict->capacity);
  if (!dict->bins)
    {
      free (dict);
      return NULL;
    }

  dict->list = list_alloc ();
  if (!dict->list)
    {
      free (dict->bins);
      free (dict);
      return NULL;
    }

  for (size_t i = 0; i < n; i++)
    {
      int res = dict_insert (dict, entities[i].key, entities[i].val);

      if (res < 0)
        {
          return NULL;
        }
    }

  return dict;
}

void
dict_destroy (Dict *dict)
{
  List *list = dict->list;

  for (size_t i = 0; i < list->count; ++i)
    {
      free (list->items[i]);
    }

  free (dict);
}

void
dict_del (Dict *dict, const char *key)
{
  size_t len = safe_strnlen (key, DICT_STR_MAX_LEN);
  size_t bins_idx = get_bins_idx (dict, HASH (key), key, len);
  int bin_val = dict->bins[bins_idx];

  if (bin_val >= 0)
    {
      dict->count--;
      dict->bins[bins_idx] = TOMBSTONE;
    }
}

int
dict_insert (Dict *dict, const char *key, void *val)
{
  if ((dict->count + 1) * 5 > dict->capacity * 4) // 80% >
    {
      if (grow_bins (dict))
        {
          return -1;
        }
    }

  size_t hash_key = HASH (key);
  size_t len = safe_strnlen (key, DICT_STR_MAX_LEN);
  size_t bin_idx = get_bins_idx (dict, hash_key, key, len);

  int bin_val = dict->bins[bin_idx];

  if (bin_val >= 0)
    {
      LIST_IDX (dict, bin_val)->val = val;
      return dict->count;
    }

  // new
  DictEntity *entity = malloc (sizeof *(entity));

  if (!entity)
    {
      return -1;
    }

  entity->hash_key = hash_key;
  entity->key = key;
  entity->len = len;
  entity->val = val;

  int list_idx = list_append (dict->list, entity);

  if (list_idx < 0)
    {
      free (entity);
      return -1;
    }

  dict->bins[bin_idx] = list_idx;
  ++dict->count;

  return dict->count;
}

DictEntity *
dict_lookup (Dict *dict, const char *key)
{
  size_t len = safe_strnlen (key, DICT_STR_MAX_LEN);
  size_t bins_idx = get_bins_idx (dict, HASH (key), key, len);
  int bin_val = dict->bins[bins_idx];

  if (bin_val >= 0)
    {
      return LIST_IDX (dict, bin_val);
    }

  return NULL;
}
