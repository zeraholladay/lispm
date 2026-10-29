#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "dict.h"
#include "str_intern.h"
#include "xalloc.h"

// Reuse the environment dictionary instead of a separate tree and two pools.
// Resizing moves entries/bins, never the strings used for symbol identity.
static Dict *interned;

static void
str_intern_init (void)
{
  static bool cleanup_registered = false;
  if (!cleanup_registered)
    {
      atexit (str_intern_destroy);
      cleanup_registered = true;
    }
  interned = dict_create (NULL, 0);
}

const char *
str_intern (const char *s, size_t len)
{
  if (!interned)
    str_intern_init ();
  DictEntity *entry = dict_lookup_n (interned, s, len);
  if (entry)
    return entry->key;

  char *key = xmalloc (len + 1);
  memcpy (key, s, len);
  key[len] = '\0';
  dict_insert_n (interned, key, len, NULL);
  return key;
}

void
str_intern_destroy (void)
{
  if (!interned)
    return;
  DictIter    iter = dict_iter (interned);
  DictEntity *entry;
  while ((entry = dict_items (&iter)))
    free ((void *)entry->key);
  dict_destroy (interned);
  interned = NULL;
}
