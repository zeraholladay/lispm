#include <stdarg.h>
#include <stdio.h>

#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>

#include "error.h"
#include "eval.h"
#include "format.h"
#include "xalloc.h"

typedef struct str_sb
{
  size_t cap, used;
  char *str;
} StrBuf;

static void appendf (StrBuf *str_sb, const char *fmt, ...);

static void fmt_nil (StrBuf *, Cell *);
static void fmt_symbol (StrBuf *, Cell *);
static void fmt_integer (StrBuf *, Cell *);
static void fmt_string (StrBuf *, Cell *);
static void fmt_cons (StrBuf *, Cell *);
static void fmt_builtin_fn (StrBuf *, Cell *);
static void fmt_lambda (StrBuf *, Cell *);
static void fmt_unknown (StrBuf *, Cell *);
static void fmt_error (StrBuf *, Cell *);

static void (*fmters[_TYPE_END + 1]) (StrBuf *, Cell *) = {
  [TYPE_NIL] = fmt_nil,         [TYPE_SYMBOL] = fmt_symbol,
  [TYPE_INTEGER] = fmt_integer, [TYPE_STRING] = fmt_string,
  [TYPE_CONS] = fmt_cons,       [TYPE_BUILTIN_FN] = fmt_builtin_fn,
  [TYPE_LAMBDA] = fmt_lambda,   [TYPE_UNKNOWN] = fmt_unknown,
  [TYPE_ERROR] = fmt_error,
};

static void
appendf (StrBuf *sb, const char *fmt, ...)
{
  bool retry = false;
  va_list ap, ap2;
  int n;

  va_start (ap, fmt);

  do
    {
      size_t avail = sb->cap - sb->used;

      va_copy (ap2, ap);
      n = vsnprintf (sb->str + sb->used, avail, fmt, ap2);
      va_end (ap2);

      if (n < 0)
        {
          va_end (ap);
          perror ("format error");
          return;
        }
      else if ((size_t)n >= avail)
        {
          size_t cap2x = sb->cap * 2; // double: 8, 16, 32, ...
          void *ptr = realloc (sb->str, cap2x * sizeof *(sb->str));
          if (!ptr)
            retry = false;
          else
            {
              sb->cap = cap2x;
              sb->str = ptr;
              retry = true;
            }
        }
      else
        {
          sb->used += (size_t)n;
          retry = false;
        }
    }
  while (retry);

  va_end (ap);
}

void
fmt_nil (StrBuf *sb, Cell *n)
{
  (void)n;
  appendf (sb, "NIL");
}

void
fmt_symbol (StrBuf *sb, Cell *n)
{
  appendf (sb, "%s", n->symbol.str);
}

void
fmt_integer (StrBuf *sb, Cell *n)
{
  appendf (sb, "%lld", n->integer);
}

void
fmt_string (StrBuf *sb, Cell *n)
{
  appendf (sb, "%s", n->string);
}

void
fmt_cons (StrBuf *sb, Cell *n)
{
  Cell *cur;
  appendf (sb, "(");

  for (cur = n; IS (cur, CONS); cur = CDR (cur))
    {
      Cell *car = CAR (cur);
      Cell *cdr = CDR (cur);

      if (car)
        {
          char *s = format (car);
          appendf (sb, "%s", s);
          free (s);
          if (CAR (cdr))
            appendf (sb, " ");
        }
    }

  if (sb->str[sb->used - 1] == ' ')
    sb->used--;

  if (!IS_NIL (cur))
    {
      appendf (sb, ".");
      char *s = format (cur);
      appendf (sb, "%s", s);
      free (s);
    }

  appendf (sb, ")");
}

void
fmt_builtin_fn (StrBuf *sb, Cell *n)
{
  return appendf (sb, "#<BUILTIN-FUNCTION %s>", n->builtin_fn->name);
}

void
fmt_lambda (StrBuf *sb, Cell *n)
{
  char *params_str = format (n->lambda.params);
  char *body_str = format (n->lambda.body);

  appendf (sb, "#<FUNCTION :LAMBDA %s %s>", params_str, body_str);

  free (params_str);
  free (body_str);
}

void
fmt_error (StrBuf *sb, Cell *c)
{
  char *cell_str = format (c->error.cell);
  appendf (sb, "*** error: %s: %s", error_messages[c->error.err_code],
           cell_str);
  free (cell_str);
}

void
fmt_unknown (StrBuf *sb, Cell *n)
{
  appendf (sb, "#<UNKNOWN %X>", n);
}

char *
format (Cell *n)
{
  size_t cap = 8;
  StrBuf sb = {
    .cap = cap,
    .used = 0,
    .str = NULL,
  };
  size_t idx = TYPE_UNKNOWN;

  sb.str = xcalloc ((&sb)->cap, sizeof *(sb.str));

  if (n)
    idx = n->type < _TYPE_END ? n->type : TYPE_UNKNOWN;

  fmters[idx](&sb, n);

  return sb.str;
}
