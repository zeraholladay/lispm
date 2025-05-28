#ifndef SYM_SAVE
#define SYM_SAVE

#include "rb_tree.h"
#include <stdlib.h>

#ifndef SYMTAB_POOL_CAPACITY
#define SYMTAB_POOL_CAPACITY 4096
#endif

#ifndef SYM_SAVE_BUMP_SIZE
#define SYM_SAVE_BUMP_SIZE 4096
#endif

const char *sym_save (const char *s, size_t s_len);

#endif
