#ifndef PARSER_INTERNAL_H
#define PARSER_INTERNAL_H

#include <stdbool.h>
#include <stddef.h>

// One-based byte coordinates; the end is exclusive. EOF is a zero-width span.
// These are parse-time locations, not metadata retained in executable Cells.
typedef struct
{
  size_t first_line;
  size_t first_column;
  size_t last_line;
  size_t last_column;
} SourceLocation;

typedef struct
{
  const char *name;
  const char *source;
  size_t      length;
  size_t      line;
  size_t      column;
  bool        has_error;
  char       *delimiters;
  size_t      delimiter_count;
  size_t      delimiter_capacity;
} ParserContext;

void parser_advance (ParserContext *context, SourceLocation *location,
                     const char *text, size_t length);
void parser_at_eof (ParserContext *context, SourceLocation *location);
void parser_report_error (ParserContext *context, SourceLocation location,
                          const char *message);
// Check original delimiter types before the lexer normalizes [] to ().
bool parser_delimiter (ParserContext *context, SourceLocation location,
                       char delimiter);

// The non-reentrant Flex scanner receives the current parse's source context
// explicitly. Its Bison semantic value and location remain generated globals.
int yylex (ParserContext *context);

#endif
