#ifndef EVAL_H
#define EVAL_H

#include "debug.h"
#include "keywords.h"
#include "lispm.h"
#include "types.h"

#define NIL (KEYWORD (NIL))
#define T (KEYWORD (T))

#define IS_NIL(nptr) (nptr == NIL)
#define LISTP(nptr) (IS_NIL (nptr) || CONSP (nptr))
#define CONSP(nptr) (IS_INST (nptr, CONS))

#define CAR(nptr) ((nptr)->cons.car)
#define CDR(nptr) ((nptr)->cons.cdr)
#define CADR(nptr) (CAR (CDR (nptr)))

#define LIST1(car, lm) (CONS (car, NIL, lm))
#define LIST2(car, cdr, lm) (CONS (car, LIST1 (cdr, lm), lm))

#define RPLACA(nptr, val)                                                     \
  do                                                                          \
    {                                                                         \
      if (CONSP (nptr))                                                       \
        CAR (nptr) = val;                                                     \
    }                                                                         \
  while (0)

#define RPLACD(nptr, val)                                                     \
  do                                                                          \
    {                                                                         \
      if (CONSP (nptr))                                                       \
        CDR (nptr) = val;                                                     \
    }                                                                         \
  while (0)

Cell *eval_append (Cell *args, LM *lm);
Cell *eval_apply (Cell *args, LM *lm);
Cell *eval_butlast (Cell *expr, LM *lm);
Cell *eval_cons (Cell *args, LM *lm);
Cell *eval_car (Cell *args, LM *lm);
Cell *eval_cdr (Cell *args, LM *lm);
Cell *eval_funcall (Cell *args, LM *lm);
Cell *eval_if (Cell *expr, LM *lm);
Cell *eval_lambda (Cell *expr, LM *lm);
Cell *eval_last (Cell *expr, LM *lm);
Cell *eval_length (Cell *args, LM *lm);
Cell *eval_list (Cell *args, LM *lm);
Cell *eval_mapcar (Cell *args, LM *lm);
Cell *eval_nth (Cell *expr, LM *lm);
Cell *eval_pair (Cell *args, LM *lm);
Cell *eval_print (Cell *args, LM *lm);
Cell *eval_reverse (Cell *args, LM *lm);
Cell *eval_set (Cell *args, LM *lm);
Cell *eval_string (Cell *args, LM *lm);
Cell *eval (Cell *form, LM *lm);
Cell *eval_list (Cell *args, LM *lm);
Cell *eval_progn (Cell *program, LM *lm);

#endif
