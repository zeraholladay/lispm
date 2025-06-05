#ifndef LISPM_UTILS_H
#define LISPM_UTILS_H

#include "lispm.h"
#include "types.h"

// sequence operations
Cell *append_inplace (Cell *list1, Cell *list2);
Cell *append_list (Cell *list1, Cell *list2, LM *lm);
Cell *butlast (Cell *args, LM *lm);
Cell *last (Cell *args, LM *lm);
size_t length (Cell *list);
Cell *mapcar (Cell *fn, Cell *arglist, LM *lm);
Cell *nth (size_t idx, Cell *list);
Cell *reverse (Cell *list, LM *lm);
Cell *reverse_inplace (Cell *list);
Cell *zip (Cell *lists, LM *lm);

// context operations
Cell *lookup (Cell *cell, LM *lm);
Cell *set (Cell *car, Cell *cdr, LM *lm);

#endif
