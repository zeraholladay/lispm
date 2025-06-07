#ifndef SYM_INTERN_H
#define SYM_INTERN_H

#include <stdlib.h>

#include "rb_tree.h"

#ifndef STR_INTERN_POOL_CAPACITY
#define STR_INTERN_POOL_CAPACITY 4096
#endif

#ifndef STR_INTERN_BUMP_SIZE
#define STR_INTERN_BUMP_SIZE 4096
#endif

const char *str_intern (const char *s, size_t s_len);

#endif
