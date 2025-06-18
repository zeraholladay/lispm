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

Parser *
parser_create (void)
{
  Parser *p  = xcalloc (1, sizeof *(p));
  p->entries = list_create ();
  return p;
}

void
parser_destroy (Parser *p)
{
  if (!p)
    return;

  List *entries = p->entries;

  for (size_t i = 0; i < entries->count; ++i)
    {
      ParserEntry *e = entries->items[i];

      switch (e->type)
        {
        case PARSER_BUF:
          free (e->buf);
          break;

        case PARSER_MMAP:
          munmap (e->buf, e->len);
          break;

        default:
          break;
        }

      free (e);
    }

  free (entries);
}

bool
parser_buf (Parser *p, LM *lm, const char *input, size_t len)
{
  ParserEntry *e = xcalloc (1, sizeof *(e));

  e->type  = PARSER_BUF;
  e->fname = NULL;
  e->buf   = xstrdup (input);
  e->len   = len;

  list_append (p->entries, e);

  yy_scan_string (e->buf);

  int status = yyparse (&p->progn, lm);

  yylex_destroy ();

  return (status == 0);
}

bool
parser_fname (Parser *p, LM *lm, const char *path)
{
  ParserEntry *e = xcalloc (1, sizeof *(e));
  size_t       len;

  char *buf = map_file (path, &len);
  if (!e->buf)
    return false;

  e->type  = PARSER_MMAP;
  e->fname = xstrdup (path);
  e->buf   = buf;
  e->len   = len;

  list_append (p->entries, e);

  YY_BUFFER_STATE yy_buf = yy_scan_bytes (e->buf, (int)len);

  int status = yyparse (&p->progn, lm);

  yy_delete_buffer (yy_buf);

  return (status == 0);
}
