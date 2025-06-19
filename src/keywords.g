%{
#include "prims.h"
#include "thunks.h"

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
"and",     &wrapped_thunks[THUNK_AND].ptr
"append",  &wrapped_thunks[THUNK_APPEND].ptr
"apply",   &wrapped_thunks[THUNK_APPLY].ptr
"butlast", &wrapped_thunks[THUNK_BUTLAST].ptr
"car",     &wrapped_thunks[THUNK_CAR].ptr
"cdr",     &wrapped_thunks[THUNK_CDR].ptr
"cons",    &wrapped_thunks[THUNK_CONS].ptr
"define",  &wrapped_thunks[THUNK_DEFINE].ptr
"eq",      &wrapped_thunks[THUNK_EQ].ptr
"eval",    &wrapped_thunks[THUNK_EVAL].ptr
"first",   &wrapped_thunks[THUNK_FIRST].ptr
"funcall", &wrapped_thunks[THUNK_FUNCALL].ptr
"gt",      &wrapped_thunks[THUNK_GT].ptr
">",       &wrapped_thunks[THUNK_GT].ptr
"if",      &wrapped_thunks[THUNK_IF].ptr
"last",    &wrapped_thunks[THUNK_LAST].ptr
"length",  &wrapped_thunks[THUNK_LENGTH].ptr
"let",     &wrapped_thunks[THUNK_LET].ptr
"lt",      &wrapped_thunks[THUNK_LT].ptr
"<",       &wrapped_thunks[THUNK_LT].ptr
"map",     &wrapped_thunks[THUNK_MAP].ptr
"nth",     &wrapped_thunks[THUNK_NTH].ptr
"not",     &wrapped_thunks[THUNK_NOT].ptr
"list",    &wrapped_thunks[THUNK_LIST].ptr
"or",      &wrapped_thunks[THUNK_OR].ptr
"print",   &wrapped_thunks[THUNK_PRINT].ptr
"progn",   &wrapped_thunks[THUNK_PROGN].ptr
"rest",    &wrapped_thunks[THUNK_REST].ptr
"reverse", &wrapped_thunks[THUNK_REVERSE].ptr
"set!",    &wrapped_thunks[THUNK_SET].ptr
"string",  &wrapped_thunks[THUNK_STRING].ptr
"add",     &wrapped_thunks[THUNK_ADD].ptr
"+",       &wrapped_thunks[THUNK_ADD].ptr
"sub",     &wrapped_thunks[THUNK_SUB].ptr
"-",       &wrapped_thunks[THUNK_SUB].ptr
"mul",     &wrapped_thunks[THUNK_MUL].ptr
"*",       &wrapped_thunks[THUNK_MUL].ptr
"div",     &wrapped_thunks[THUNK_DIV].ptr
"/",       &wrapped_thunks[THUNK_DIV].ptr
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
