#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "bison.h"
#include "flex.h"
#include "lm.h"
#include "parser.h"
#include "types.h"
#include "xalloc.h"

typedef enum
{
  PARSER_BUF,
  PARSER_MMAP,
} ParserEnum;

typedef struct node
{
  ParserEnum   type;
  char        *fname;
  char        *fbuf;
  size_t       len;
  struct node *next;
} Node;

static Node *head = NULL;
static Node *tail = NULL;

static void
ll_append (Node *n)
{
  n->next = NULL;

  if (tail)
    tail->next = n;
  else
    {
      head = n;
      tail = n;
    }
}

static char *
map_file (const char *path, size_t *len)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    {
      perror ("open failed");
      return NULL;
    }

  struct stat st;
  if (fstat (fd, &st) < 0)
    {
      perror ("fstat");
      close (fd);
      return NULL;
    }

  size_t size = (size_t)st.st_size;
  if (size == 0)
    {
      perror ("file is empty");
      close (fd);
      *len = 0;
      return NULL;
    }

  // map read-only, private:
  char *data = mmap (NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
  close (fd);

  if (data == MAP_FAILED)
    {
      perror ("map failed");
      return NULL;
    }

  *len = size;
  return data;
}

void *
parser_loads (const char *str, size_t len)
{
  Node *n = xmalloc (sizeof *(n));

  n->type  = PARSER_BUF;
  n->fname = NULL;
  n->fbuf  = xstrdup (str);
  n->len   = len;

  ll_append (n);

  return n;
}

void *
parser_load (const char *fname)
{
  Node  *n = xcalloc (1, sizeof *(n));
  size_t len;

  char *buf = map_file (fname, &len);
  if (!buf)
    return NULL;

  n->type  = PARSER_MMAP;
  n->fname = xstrdup (fname);
  n->fbuf  = buf;
  n->len   = len;

  ll_append (n);

  return n;
}

void
parser_destroy (void)
{
  for (Node *n = head; n; n = n->next)
    {
      switch (n->type)
        {
        case PARSER_BUF:
          free (n->fbuf);
          break;

        case PARSER_MMAP:
          free (n->fname);
          munmap (n->fbuf, n->len);
          break;

        default:
          break;
        }

      free (n);
    }

  head = tail = NULL;
}

bool
parser_parse_bytes (void *ptr, Cell **progn, LM *lm)
{
  Node *n = ptr;

  YY_BUFFER_STATE yy_buf = yy_scan_bytes (n->fbuf, (int)n->len);

  int status = yyparse (progn, (void **) &n->fbuf, lm);

  yy_delete_buffer (yy_buf);

  return (status == 0);
}

void
parser_print_loc (Cell *c)
{
  if (!c || !c->loc.parser_ptr)
    {
      fprintf (stderr, "Not valid to print\n");
      return;
    }

  LType *ltype = &c->loc;

  Node *n = (Node *)((char *)*ltype->parser_ptr - offsetof (Node, fbuf));

  char  *fbuf  = n->fbuf;
  size_t len   = n->len;
  char  *fname = n->fname ? n->fname : "<stdin>";

  int first_line   = ltype->first_line;
  int first_column = ltype->first_column;
  int last_line    = ltype->last_line;
  int last_column  = ltype->last_column;

  char *p   = fbuf;
  char *end = fbuf + len;

  // Find start of first_line
  for (int line = 1; line < first_line; ++line)
    {
      p = memchr (p, '\n', end - p);
      if (!p)
        return; // file has fewer than first_line lines
      ++p;
    }

  // Advance first_column (check bounds)
  if (p + first_column > end)
    return;

  char *start_ptr = p + first_column;

  // Find start of last_line
  p = fbuf;
  for (int line = 1; line < last_line; ++line)
    {
      p = memchr (p, '\n', end - p);
      if (!p)
        return;
      ++p;
    }

  // Advance last_column (check bounds)
  if (p + last_column > end)
    return;

  char *end_ptr = p + last_column;

  if (end_ptr < start_ptr)
    return; // inverted region

  // Allocate & copy
  size_t str_len = end_ptr - start_ptr;
  char  *str     = xmalloc (str_len + 1);
  memcpy (str, start_ptr, str_len);
  str[str_len] = '\0';

  fprintf (stderr,
           "Location: %s\n"
           "Line: %d\n"
           "File: %s\n",
           str, first_line, fname);
}
