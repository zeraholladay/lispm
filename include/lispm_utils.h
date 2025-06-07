#ifndef LISPM_UTILS_H
#define LISPM_UTILS_H

#include "lispm.h"
#include "types.h"

// sequence operations
Cell *append_inplace (Cell *lst1, Cell *lst2);
Cell *append_list (LM *lm, Cell *lst1, Cell *lst2);
Cell *butlast (LM *lm, Cell *args);
Cell *last (LM *lm, Cell *args);
size_t length (Cell *lst);
Cell *mapcar (LM *lm, Cell *fn, Cell *arglst);
Cell *nth (size_t idx, Cell *lst);
Cell *reverse (LM *lm, Cell *lst);
Cell *reverse_inplace (Cell *lst);
Cell *zip (LM *lm, Cell *lsts);

// context operations
Cell *lookup (LM *lm, Cell *cell);
Cell *set (LM *lm, Cell *car, Cell *cdr);

#endif
