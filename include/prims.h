#ifndef PRIMS_H
#define PRIMS_H

#include "lm.h"
#include "types.h"

extern Cell _nil;
extern Cell _t;
extern Cell _quote;

#define NIL (&_nil)
#define T (&_t)
#define QUOTE (&_quote)

#define NILP(nptr) (nptr == NIL)
#define LISTP(nptr) (NILP (nptr) || CONSP (nptr))
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

// sequence operations
Cell *append_inplace (Cell *lst1, Cell *lst2);
Cell *append_list (LM *lm, Cell *lst1, Cell *lst2);
Cell *butlast (LM *lm, Cell *args);
Cell *last (LM *lm, Cell *args);
size_t length (Cell *lst);
Cell *nth (size_t idx, Cell *lst);
Cell *reverse (LM *lm, Cell *lst);
Cell *reverse_inplace (Cell *lst);
Cell *zip (LM *lm, Cell *lsts);

// context operations
Cell *lookup (LM *lm, Cell *cell);
Cell *set (LM *lm, Cell *car, Cell *cdr);

#endif
