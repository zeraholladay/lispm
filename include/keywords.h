#ifndef KEYWORDS_H
#define KEYWORDS_H

#include <stddef.h>

#include "types.h"

// keywords.g
Cell       *keyword_lookup (const char *str, size_t len);
const char *is_keyword_strncmp (const char *text, int state);

#endif
