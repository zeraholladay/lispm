#ifndef EVAL_H
#define EVAL_H

#include "context.h"
#include "debug.h"
#include "keywords.h"
#include "types.h"

#define NIL (KEYWORD (NIL))
#define T (KEYWORD (T))

#define IS_NIL(nptr) (nptr == NIL)
#define LISTP(nptr) (IS_NIL (nptr) || CONSP (nptr))
#define CONSP(nptr) (IS (nptr, CONS))

#define CAR(nptr) ((nptr)->cons.car)
#define CDR(nptr) ((nptr)->cons.cdr)
#define CADR(nptr) (CAR (CDR (nptr)))
#define FIRST(nptr) ((nptr)->cons.car)
#define REST(nptr) ((nptr)->cons.cdr)

#define CONS(car, cdr, ctx) (cons_cons (&CTX_POOL (ctx), car, cdr))
#define LIST1(car, ctx) (CONS (car, NIL, ctx))
#define LIST2(car, cdr, ctx) (CONS (car, LIST1 (cdr, ctx), ctx))

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

Cell *eval_append (Cell *args, Context *ctx);
Cell *eval_apply (Cell *args, Context *ctx);
Cell *eval_butlast (Cell *expr, Context *ctx);
Cell *eval_cons (Cell *args, Context *ctx);
Cell *eval_car (Cell *args, Context *ctx);
Cell *eval_cdr (Cell *args, Context *ctx);
Cell *eval_funcall (Cell *args, Context *ctx);
Cell *eval_if (Cell *expr, Context *ctx);
Cell *eval_lambda (Cell *expr, Context *ctx);
Cell *eval_last (Cell *expr, Context *ctx);
Cell *eval_len (Cell *args, Context *ctx);
Cell *eval_list (Cell *args, Context *ctx);
Cell *eval_mapcar (Cell *args, Context *ctx);
Cell *eval_nth (Cell *expr, Context *ctx);
Cell *eval_pair (Cell *args, Context *ctx);
Cell *eval_print (Cell *args, Context *ctx);
Cell *eval_reverse (Cell *args, Context *ctx);
Cell *eval_set (Cell *args, Context *ctx);
Cell *eval_string (Cell *args, Context *ctx);
Cell *eval (Cell *form, Context *ctx);
Cell *eval_list (Cell *args, Context *ctx);
Cell *eval_progn (Cell *program, Context *ctx);

#endif
