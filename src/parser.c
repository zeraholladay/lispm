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
  char        *data;
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
map_file (const char *path, size_t *out_len)
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

  size_t len = (size_t)st.st_size;
  if (len == 0)
    {
      perror ("file is empty");
      close (fd);
      *out_len = 0;
      return NULL;
    }

  // map read-only, private:
  char *data = mmap (NULL, len, PROT_READ, MAP_PRIVATE, fd, 0);
  close (fd);

  if (data == MAP_FAILED)
    {
      perror ("map failed");
      return NULL;
    }

  *out_len = len;
  return data;
}

void *
parser_loads (const char *str, size_t len)
{
  Node *n = xmalloc (sizeof *(n));

  n->type  = PARSER_BUF;
  n->fname = NULL;
  n->data  = xstrdup (str);
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
  n->data  = buf;
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
          free (n->data);
          break;

        case PARSER_MMAP:
          free (n->fname);
          munmap (n->data, n->len);
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

  YY_BUFFER_STATE yy_buf = yy_scan_bytes (n->data, (int)n->len);

  int status = yyparse (progn, lm);

  yy_delete_buffer (yy_buf);

  return (status == 0);
}
