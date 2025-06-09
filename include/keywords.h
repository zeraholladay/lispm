#ifndef KEYWORDS_H
#define KEYWORDS_H

#include <stddef.h>

#include "types.h"

#define KEYWORD(name) keyword_lookup (#name, sizeof (#name) - 1)
#define KEYWORD_LIT(str) keyword_lookup (str, sizeof (str) - 1)

// keywords.gperf
struct Cell *keyword_lookup (const char *str, size_t len);
const char *is_keyword_strncmp (const char *text, int state);

#endif
