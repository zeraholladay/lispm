#ifndef LISPM_UTILS_H
#define LISPM_UTILS_H

#include "lispm.h"
#include "types.h"

// sequence operations
Cell *append_inplace (Cell *list1, Cell *list2);
Cell *append_list (LM *lm, Cell *list1, Cell *list2);
Cell *butlast (LM *lm, Cell *args);
Cell *last ( LM *lm, Cell *args);
size_t length (Cell *list);
Cell *mapcar (LM *lm, Cell *fn, Cell *arglist);
Cell *nth (size_t idx, Cell *list);
Cell *reverse (LM *lm, Cell *list);
Cell *reverse_inplace (Cell *list);
Cell *zip (LM *lm, Cell *lists);

// context operations
Cell *lookup (LM *lm, Cell *cell);
Cell *set (LM *lm, Cell *car, Cell *cdr);

#endif
