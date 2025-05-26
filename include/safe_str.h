#ifndef SAFE_STR
#define SAFE_STR

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "xalloc.h"

#define STR_LITERAL_DUP(lit) safe_strndup (lit, sizeof (lit) - 1)

inline static unsigned long
djb2 (const char *str)
{
  unsigned long hash = 5381;
  int c;

  while ((c = *str++))
    {
      hash = ((hash << 5) + hash) + c; // hash * 33 + c
    }

  return hash;
}

inline static size_t
safe_strnlen (const char *s, size_t maxlen)
{
  assert (s);
  size_t i;
  for (i = 0; i < maxlen && s[i]; i++)
    continue;
  return i;
}

// usage note: safe_strndup(s1, len)
inline static char *
safe_strndup (char const *s, size_t n)
{
  assert (s);
  size_t len = safe_strnlen (s, n);
  char *new = (char *)xmalloc (len + 1);

  if (new == NULL)
    return NULL;

  new[len] = '\0';
  return memcpy (new, s, len);
}

// usage note: safe_strncmp_minlen(s1, s2, len + 1)
inline static int
safe_strncmp_minlen (const char *s1, const char *s2, size_t n)
{
  assert (s1 && s2 && n > 1 && (s1[n - 1] == '\0' || s2[n - 1] == '\0'));

  size_t len1 = safe_strnlen (s1, n);
  size_t len2 = safe_strnlen (s2, n);

  int cmp = memcmp (s1, s2, len1 < len2 ? len1 : len2);
  if (cmp != 0 || len1 == len2)
    return cmp;
  return (len1 < len2) ? -1 : 1;
}

#endif
