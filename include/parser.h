#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>

#include "list.h"
#include "lm.h"
#include "types.h"

typedef enum
{
  PARSER_BUF,
  PARSER_MMAP,
} ParserEnum;

typedef struct parser_entry
{
  ParserEnum type;
  char      *fname;
  char      *buf;
  size_t     len;
} ParserEntry;

typedef struct parser
{
  List *entries;
  Cell *progn;
} Parser;

Parser *parser_create (void);
void    parser_destory (Parser *p);
bool    parser_buf (Parser *p, LM *lm, const char *input, size_t len);
bool    parser_fname (Parser *p, LM *lm, const char *path);

#endif
