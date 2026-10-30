#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "builtins.h"
#include "fmt.h"
#include "prims.h"
#include "xalloc.h"

typedef struct str_sb
{
  size_t cap, used;
  char  *str;
} StrBuf;

static void appendf (StrBuf *str_sb, const char *fmt, ...);

static void fmt_nil (StrBuf *, Cell *);
static void fmt_symbol (StrBuf *, Cell *);
static void fmt_integer (StrBuf *, Cell *);
static void fmt_float (StrBuf *, Cell *);
static void fmt_string (StrBuf *, Cell *);
static void fmt_cons (StrBuf *, Cell *);
static void fmt_builtin (StrBuf *, Cell *);
static void fmt_lambda (StrBuf *, Cell *);
static void fmt_unknown (StrBuf *, Cell *);

static void (*fmters[CELL_TYPE_COUNT]) (StrBuf *, Cell *) = {
  [CELL_TYPE_NIL] = fmt_nil,         [CELL_TYPE_SYMBOL] = fmt_symbol,
  [CELL_TYPE_INTEGER] = fmt_integer, [CELL_TYPE_FLOAT] = fmt_float,
  [CELL_TYPE_STRING] = fmt_string,   [CELL_TYPE_CONS] = fmt_cons,
  [CELL_TYPE_BUILTIN] = fmt_builtin, [CELL_TYPE_LAMBDA] = fmt_lambda,
  [CELL_TYPE_UNKNOWN] = fmt_unknown, [CELL_TYPE_ENV] = fmt_unknown,
};

static void
appendf (StrBuf *sb, const char *fmt, ...)
{
  bool    retry = false;
  va_list ap, ap2;
  int     n;

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
          void  *ptr   = realloc (sb->str, cap2x * sizeof *(sb->str));
          if (!ptr)
            retry = false;
          else
            {
              sb->cap = cap2x;
              sb->str = ptr;
              retry   = true;
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
  appendf (sb, "nil");
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
fmt_float (StrBuf *sb, Cell *n)
{
  appendf (sb, "%f", n->floating);
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

  for (cur = n; cell_is_pair (cur); cur = CDR (cur))
    {
      Cell *car = CAR (cur);
      Cell *cdr = CDR (cur);

      if (car)
        {
          char *s = format (car);
          appendf (sb, "%s", s);
          free (s);
          if (cell_is_pair (cdr))
            appendf (sb, " ");
        }
    }

  if (!cell_is_nil (cur))
    {
      appendf (sb, " . ");
      char *s = format (cur);
      appendf (sb, "%s", s);
      free (s);
    }

  appendf (sb, ")");
}

void
fmt_builtin (StrBuf *sb, Cell *n)
{
  return appendf (sb, "#<BUILTIN %s>", builtin_name (n));
}

void
fmt_lambda (StrBuf *sb, Cell *n)
{
  char *params_str = format (n->lambda.params);
  char *body_str   = format (n->lambda.body);

  appendf (sb, "#<LAMBDA %s %s>", params_str, body_str);

  free (params_str);
  free (body_str);
}

void
fmt_unknown (StrBuf *sb, Cell *n)
{
  appendf (sb, "#<UNKNOWN %p>", (void *)n);
}

char *
format (Cell *n)
{
  size_t cap = 8;
  StrBuf sb  = {
     .cap  = cap,
     .used = 0,
     .str  = NULL,
  };
  size_t idx = CELL_TYPE_UNKNOWN;

  sb.str = xcalloc ((&sb)->cap, sizeof *(sb.str));

  if (n)
    idx = (unsigned)n->type < CELL_TYPE_COUNT ? n->type : CELL_TYPE_UNKNOWN;

  fmters[idx](&sb, n);

  return sb.str;
}
