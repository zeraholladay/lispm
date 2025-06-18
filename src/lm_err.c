#include <stdarg.h>
#include <stdio.h>

#include "lm.h"
#include "lm_err.h"
#include "prims.h"

static const char *err_msg[] = {
  [ERR_INTERNAL]          = "Internal error occurred",
  [ERR_NOT_A_FN]          = "No such function",
  [ERR_SYM_NOT_FOUND]     = "Could not resolve symbol",
  [ERR_INVALID_ARG]       = "Invalid argument type or value",
  [ERR_MISSING_ARG]       = "Missing required argument",
  [ERR_ARG_TYPE_MISMATCH] = "Argument type mismatch",
  [ERR_ARG_OUT_OF_RANGE]  = "Argument value out of range",
  [ERR_UNEXPECTED_ARG]    = "Unexpected argument provided",
  [ERR_INVALID_ARG_LEN]   = "Invalid argument length",
  [ERR_NULL_ARG]          = "Argument cannot be null",
  [ERR_ARG_NOT_ITER]      = "Argument is not iterable when expected",
  [ERR_DIVISION_BY_0]     = "Division by zero",
  [ERR_OVERFLOW]          = "Stack or control‐stack overflow",
  [ERR_UNDERFLOW]         = "Stack or control‐stack underflow",
};

static void
lm_err_vprint (Err code, const char *fmt, va_list ap)
{
  fprintf (stderr, "***error: %s: ", err_msg[code]);
  vfprintf (stderr, fmt, ap);
  fputc ('\n', stderr);
}

bool lm_err_bool (LM *lm, Err code, const char *fmt, ...)
    __attribute__ ((format (printf, 3, 4)));

bool
lm_err_bool (LM *lm, Err code, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  lm_err_vprint (code, fmt, ap);
  va_end (ap);

  lm->err_bool = true;
  return false;
}

Cell *lm_err_nil (LM *lm, Err code, const char *fmt, ...)
    __attribute__ ((format (printf, 3, 4)));

Cell *
lm_err_nil (LM *lm, Err code, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  lm_err_vprint (code, fmt, ap);
  va_end (ap);

  lm->err_bool = true;
  return NIL;
}

void *lm_err_null (LM *lm, Err code, const char *fmt, ...)
    __attribute__ ((format (printf, 3, 4)));

void *
lm_err_null (LM *lm, Err code, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  lm_err_vprint (code, fmt, ap);
  va_end (ap);

  lm->err_bool = true;
  return NULL;
}
