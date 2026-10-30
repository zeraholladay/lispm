#ifndef SYM_INTERN_H
#define SYM_INTERN_H

#include <stddef.h>

const char *str_intern (const char *s, size_t s_len);
void        str_intern_destroy (void);

#endif
