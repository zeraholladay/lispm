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
#define CONSP(nptr) (IS_CONS (nptr))

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

Node *eval_append (Node *args, Context *ctx);
Node *eval_apply (Node *args, Context *ctx);
Node *eval_butlast (Node *expr, Context *ctx);
Node *eval_cons (Node *args, Context *ctx);
Node *eval_car (Node *args, Context *ctx);
Node *eval_cdr (Node *args, Context *ctx);
Node *eval_funcall (Node *args, Context *ctx);
Node *eval_if (Node *expr, Context *ctx);
Node *eval_lambda (Node *expr, Context *ctx);
Node *eval_last (Node *expr, Context *ctx);
Node *eval_len (Node *args, Context *ctx);
Node *eval_list (Node *args, Context *ctx);
Node *eval_mapcar (Node *args, Context *ctx);
Node *eval_nth (Node *expr, Context *ctx);
Node *eval_pair (Node *args, Context *ctx);
Node *eval_print (Node *args, Context *ctx);
Node *eval_reverse (Node *args, Context *ctx);
Node *eval_set (Node *args, Context *ctx);
Node *eval_str (Node *args, Context *ctx);
Node *eval (Node *form, Context *ctx);
Node *eval_list (Node *args, Context *ctx);
Node *eval_progn (Node *program, Context *ctx);

#endif
