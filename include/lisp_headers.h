#ifndef LISP_HEADERS_H
#define LISP_HEADERS_H

#include "err.h"
#include "fmt.h"
#include "keywords.h"
#include "lm.h"
#include "thunks.h"
#include "types.h"
#include "utils.h"

#define NIL (KEYWORD (NIL))
#define T (KEYWORD (T))

#define NILP(nptr) (nptr == NIL)
#define LISTP(nptr) (NILP (nptr) || CONSP (nptr))
#define CONSP(nptr) (IS_INST (nptr, CONS))

#define CAR(nptr) ((nptr)->cons.car)
#define CDR(nptr) ((nptr)->cons.cdr)
#define CADR(nptr) (CAR (CDR (nptr)))

#define LIST1(car, lm) (CONS (car, NIL, lm))
#define LIST2(car, cdr, lm) (CONS (car, LIST1 (cdr, lm), lm))

#endif
