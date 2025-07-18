#ifndef PRIMS_H
#define PRIMS_H

#include "lm.h"
#include "palloc.h"
#include "types.h"

typedef struct prim_wrapper PrimWrapper;

extern PrimWrapper wrapper_nil;
extern PrimWrapper wrapped_t;
extern PrimWrapper wrapper_quote;

#define NIL   (&wrapper_nil.ptr)
#define T     (&wrapped_t.ptr)
#define QUOTE (&wrapper_quote.ptr)

#define NILP(x)  (x == NIL)
#define LISTP(x) (NILP (x) || CONSP (x))
#define CONSP(x) (IS_INST (x, CONS))

#define CAR(x)   ((x)->cons.car)
#define CDR(x)   ((x)->cons.cdr)
#define CAAR(x)  (CAR (CAR (x)))
#define CADR(x)  (CAR (CDR (x)))
#define CDDR(x)  (CDR (CDR (x)))
#define CAADR(x) (CAR (CADR (x)))
#define CADDR(x) (CAR (CDDR (x)))
#define CDDDR(x) (CDR (CDDR (x)))

#define LIST1(car, lm)      (CONS (car, NIL, lm))
#define LIST2(car, cdr, lm) (CONS (car, LIST1 (cdr, lm), lm))

#define RPLACA(x, val)                                                        \
  do                                                                          \
    {                                                                         \
      if (CONSP (x))                                                          \
        CAR (x) = val;                                                        \
    }                                                                         \
  while (0)

#define RPLACD(x, val)                                                        \
  do                                                                          \
    {                                                                         \
      if (CONSP (x))                                                          \
        CDR (x) = val;                                                        \
    }                                                                         \
  while (0)

struct prim_wrapper
{
  PALLOC_WRAPPER_FIELDS (PrimWrapper)
  Cell ptr;
};

Cell  *append_inplace (Cell *lst1, Cell *lst2);
Cell  *append_list (LM *lm, Cell *lst1, Cell *lst2);
Cell  *butlast (LM *lm, Cell *args);
Cell  *last (LM *lm, Cell *args);
size_t length (Cell *lst);
Cell  *nth (size_t idx, Cell *lst);
Cell  *reverse (LM *lm, Cell *lst);
Cell  *reverse_inplace (Cell *lst);
Cell  *zip (LM *lm, Cell *lsts);

#endif
