#ifndef EVAL_H
#define EVAL_H

#include "ctx.h"
#include "keywords.h"
#include "types.h"

#define NIL (KEYWORD (NIL))
#define T (KEYWORD (T))

#define IS_NIL(nptr) (nptr == NIL)
#define LISTP(nptr) (IS_NIL (nptr) || CONSP (nptr))
#define CONSP(nptr) (IS_CONS (nptr))

#define CAR(nptr) ((nptr)->cons.car)
#define CDR(nptr) ((nptr)->cons.cdr)
#define CADR(nptr) (CAR (CDR (nptr)))
#define FIRST(nptr) ((nptr)->cons.car)
#define REST(nptr) ((nptr)->cons.cdr)

#define CONS(car, cdr, ctx) (cons_cons (car, cdr, ctx))
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

Node *eval_append (Node *args, Ctx *ctx);
Node *eval_apply (Node *args, Ctx *ctx);
Node *eval_butlast (Node *expr, Ctx *ctx);
Node *eval_cons (Node *args, Ctx *ctx);
Node *eval_car (Node *args, Ctx *ctx);
Node *eval_cdr (Node *args, Ctx *ctx);
Node *eval_funcall (Node *args, Ctx *ctx);
Node *eval_if (Node *expr, Ctx *ctx);
Node *eval_lambda (Node *expr, Ctx *ctx);
Node *eval_last (Node *expr, Ctx *ctx);
Node *eval_len (Node *args, Ctx *ctx);
Node *eval_list (Node *args, Ctx *ctx);
Node *eval_mapcar (Node *args, Ctx *ctx);
Node *eval_nth (Node *expr, Ctx *ctx);
Node *eval_pair (Node *args, Ctx *ctx);
Node *eval_print (Node *args, Ctx *ctx);
Node *eval_reverse (Node *args, Ctx *ctx);
Node *eval_set (Node *args, Ctx *ctx);
Node *eval_str (Node *args, Ctx *ctx);
Node *eval (Node *form, Ctx *ctx);
Node *eval_list (Node *args, Ctx *ctx);
Node *eval_progn (Node *program, Ctx *ctx);

#endif
