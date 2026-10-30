#ifndef PRIMS_H
#define PRIMS_H

#include "lm.h"
#include "palloc.h"
#include "types.h"

typedef struct prim_wrapper PrimWrapper;

struct prim_wrapper
{
  PALLOC_WRAPPER_FIELDS (PrimWrapper)
  Cell ptr;
};

extern PrimWrapper wrapper_nil;
extern PrimWrapper wrapped_t;
extern PrimWrapper wrapper_quote;

#define NIL   (&wrapper_nil.ptr)
#define T     (&wrapped_t.ptr)
#define QUOTE (&wrapper_quote.ptr)

static inline bool
cell_is_nil (const Cell *cell)
{
  return cell == NIL;
}

static inline bool
cell_is_pair_or_nil (const Cell *cell)
{
  return cell_is_nil (cell) || cell_is_pair (cell);
}

#define CAR(x)   ((x)->cons.car)
#define CDR(x)   ((x)->cons.cdr)
#define CAAR(x)  (CAR (CAR (x)))
#define CADR(x)  (CAR (CDR (x)))
#define CDDR(x)  (CDR (CDR (x)))
#define CAADR(x) (CAR (CADR (x)))
#define CADDR(x) (CAR (CDDR (x)))
#define CDDDR(x) (CDR (CDDR (x)))

static inline Cell *
cell_list1 (LM *lm, Cell *value)
{
  return cell_make_pair (lm, value, NIL);
}

static inline Cell *
cell_list2 (LM *lm, Cell *first, Cell *second)
{
  return cell_make_pair (lm, first, cell_list1 (lm, second));
}

static inline void
cell_set_car (Cell *cell, Cell *value)
{
  if (cell_is_pair (cell))
    CAR (cell) = value;
}

static inline void
cell_set_cdr (Cell *cell, Cell *value)
{
  if (cell_is_pair (cell))
    CDR (cell) = value;
}

bool   cell_is_proper_list (const Cell *list);
size_t length (Cell *lst);
Cell  *reverse_inplace (Cell *lst);

#endif
