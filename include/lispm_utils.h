#ifndef LISPM_UTILS_H
#define LISPM_UTILS_H

#include "context.h"
#include "types.h"

// funcalls
Cell *funcall (Cell *fn, Cell *arglist, Context *ctx);
Cell *funcall_builtin (Cell *fn, Cell *args, Context *ctx);
Cell *funcall_lambda (Cell *fn, Cell *args, Context *ctx);

// sequence operations
Cell *append_inplace (Cell *list1, Cell *list2);
Cell *append_list (Cell *list1, Cell *list2, Context *ctx);
Cell *butlast (Cell *args, Context *ctx);
Cell *last (Cell *args, Context *ctx);
size_t length (Cell *list);
Cell *mapcar (Cell *fn, Cell *arglist, Context *ctx);
Cell *nth (size_t idx, Cell *list);
Cell *reverse (Cell *list, Context *ctx);
Cell *reverse_inplace (Cell *list);
Cell *zip (Cell *lists, Context *ctx);

// context operations
Cell *lookup (Cell *cell, Context *ctx);
Cell *set (Cell *car, Cell *cdr, Context *ctx);

#endif
