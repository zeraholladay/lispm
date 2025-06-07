#ifndef LISP_BUILTINS_H
#define LISP_BUILTINS_H

#include "lisp_mach.h"
#include "lisp_types.h"

Cell *fn_append (LM *lm, Cell *lst);
Cell *fn_butlast (LM *lm, Cell *lst);
Cell *fn_cons (LM *lm, Cell *args);
Cell *fn_car (LM *lm, Cell *args);
Cell *fn_cdr (LM *lm, Cell *args);
Cell *fn_last (LM *lm, Cell *lst);
Cell *fn_length (LM *lm, Cell *lst);
Cell *fn_list (LM *lm, Cell *args);
Cell *fn_mapcar (LM *lm, Cell *args);
Cell *fn_nth (LM *lm, Cell *args);
Cell *fn_print (LM *lm, Cell *args);
Cell *fn_reverse (LM *lm, Cell *lst);
Cell *fn_set (LM *lm, Cell *args);
Cell *fn_string (LM *lm, Cell *args);
// boolean
Cell *fn_eq (LM *lm, Cell *args);
Cell *fn_not (LM *lm, Cell *args);
// math
Cell *fn_gt (LM *lm, Cell *args);
Cell *fn_lt (LM *lm, Cell *args);
Cell *fn_add (LM *lm, Cell *args);
Cell *fn_sub (LM *lm, Cell *args);
Cell *fn_mul (LM *lm, Cell *args);
Cell *fn_div (LM *lm, Cell *args);

#endif
