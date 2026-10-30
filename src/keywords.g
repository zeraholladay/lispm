%{
#include "prims.h"
#include "builtins.h"

typedef struct Keyword
{
  const char *name;
  Cell *cell;
} Keyword;

%}

%enum
%includes
%global-table
%struct-type
%ignore-case
%define word-array-name keyword_table
%define constants-prefix GPERF_
%define slot-name name

struct Keyword;
%%
"nil",     NIL
"t",       T
"quote",   QUOTE
"'",       QUOTE
"and",     &builtin_cells[BUILTIN_AND].ptr
"apply",   &builtin_cells[BUILTIN_APPLY].ptr
"car",     &builtin_cells[BUILTIN_CAR].ptr
"cdr",     &builtin_cells[BUILTIN_CDR].ptr
"cons",    &builtin_cells[BUILTIN_CONS].ptr
"define",  &builtin_cells[BUILTIN_DEFINE].ptr
"eq",      &builtin_cells[BUILTIN_EQ].ptr
"=",       &builtin_cells[BUILTIN_NUM_EQ].ptr
"error",   &builtin_cells[BUILTIN_ERROR].ptr
"integer?",   &builtin_cells[BUILTIN_INTEGER_P].ptr
"number?",    &builtin_cells[BUILTIN_NUMBER_P].ptr
"pair?",      &builtin_cells[BUILTIN_PAIR_P].ptr
"procedure?", &builtin_cells[BUILTIN_PROCEDURE_P].ptr
"symbol?",    &builtin_cells[BUILTIN_SYMBOL_P].ptr
"eval",    &builtin_cells[BUILTIN_EVAL].ptr
"funcall", &builtin_cells[BUILTIN_FUNCALL].ptr
"gt",      &builtin_cells[BUILTIN_GT].ptr
">",       &builtin_cells[BUILTIN_GT].ptr
"if",      &builtin_cells[BUILTIN_IF].ptr
"let",     &builtin_cells[BUILTIN_LET].ptr
"lt",      &builtin_cells[BUILTIN_LT].ptr
"<",       &builtin_cells[BUILTIN_LT].ptr
"or",      &builtin_cells[BUILTIN_OR].ptr
"print",   &builtin_cells[BUILTIN_PRINT].ptr
"progn",   &builtin_cells[BUILTIN_PROGN].ptr
"set!",    &builtin_cells[BUILTIN_SET].ptr
"string",  &builtin_cells[BUILTIN_STRING].ptr
"add",     &builtin_cells[BUILTIN_ADD].ptr
"+",       &builtin_cells[BUILTIN_ADD].ptr
"sub",     &builtin_cells[BUILTIN_SUB].ptr
"-",       &builtin_cells[BUILTIN_SUB].ptr
"mul",     &builtin_cells[BUILTIN_MUL].ptr
"*",       &builtin_cells[BUILTIN_MUL].ptr
"div",     &builtin_cells[BUILTIN_DIV].ptr
"/",       &builtin_cells[BUILTIN_DIV].ptr
%%

Cell *
keyword_lookup (const char *str, size_t len)
{
  Keyword *keyword = in_word_set(str, len);

  if (!keyword)
    return NULL;

  return keyword->cell;
}

const char *
is_keyword_strncmp (const char *text, int state)
{
  static int idx;
  static size_t len;

  if (state == 0)
    {
      idx = 0;
      len = strlen (text);
    }

  while (idx < GPERF_TOTAL_KEYWORDS)
    {
      const char *name = keyword_table[idx++].name;
      if (name[0] == '\0')
        continue;
      else if (!strncasecmp (name, text, len))
        return name;
    }

  return NULL;
}
