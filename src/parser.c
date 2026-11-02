#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bison.h"
#include "flex.h"
#include "parser.h"
#include "parser_internal.h"
#include "prims.h"
#include "xalloc.h"

// yy_scan_bytes takes an int and needs two extra end-of-buffer bytes.
#define PARSER_MAX_SOURCE ((size_t)INT_MAX - 2)

void
parser_at_eof (ParserContext *context, SourceLocation *location)
{
  *location = (SourceLocation){ context->line, context->column, context->line,
                                context->column };
}

void
parser_advance (ParserContext *context, SourceLocation *location,
                const char *text, size_t length)
{
  parser_at_eof (context, location);
  // Flex calls this for every match, including ignored whitespace/comments.
  // Count LF once for CRLF too; the preceding CR is just another source byte.
  for (size_t i = 0; i < length; ++i)
    {
      if (text[i] == '\n')
        {
          ++context->line;
          context->column = 1;
        }
      else
        ++context->column;
    }
  location->last_line   = context->line;
  location->last_column = context->column;
}

// Render or measure a source slice using the same display rules. Reported
// columns count bytes; tabs use eight-column stops only in the excerpt. Escape
// control bytes so even an accidental NUL cannot truncate or corrupt a
// message.
static size_t
render_slice (FILE *output, const char *text, size_t length, size_t column)
{
  for (size_t i = 0; i < length; ++i)
    {
      unsigned char byte = (unsigned char)text[i];
      if (byte == '\t')
        {
          size_t spaces = 8 - column % 8;
          if (output)
            for (size_t j = 0; j < spaces; ++j)
              fputc (' ', output);
          column += spaces;
        }
      else if (byte < 32 || byte == 127)
        {
          if (output)
            fprintf (output, "\\x%02X", byte);
          column += 4;
        }
      else
        {
          if (output)
            fputc (byte, output);
          ++column;
        }
    }
  return column;
}

void
parser_report_error (ParserContext *context, SourceLocation location,
                     const char *message)
{
  // Lexical errors are reported before Bison sees the invalid token. Suppress
  // the ensuing generic syntax error so each failure has one useful
  // diagnostic.
  if (context->has_error)
    return;
  context->has_error = true;
  fprintf (stderr, "%s:%zu:%zu: error: %s\n", context->name,
           location.first_line, location.first_column, message);

  size_t start = 0;
  for (size_t line = 1; line < location.first_line && start < context->length;)
    if (context->source[start++] == '\n')
      ++line;
  size_t end = start;
  while (end < context->length && context->source[end] != '\n')
    ++end;
  if (end < context->length && end > start && context->source[end - 1] == '\r')
    --end;

  fputs ("  ", stderr);
  render_slice (stderr, context->source + start, end - start, 0);
  fputc ('\n', stderr);

  size_t offset = location.first_column > 0 ? location.first_column - 1 : 0;
  if (offset > end - start)
    offset = end - start;
  size_t padding = render_slice (NULL, context->source + start, offset, 0);
  size_t span    = end - start - offset;
  if (location.last_line == location.first_line)
    {
      size_t requested = location.last_column > location.first_column
                             ? location.last_column - location.first_column
                             : 0;
      if (requested < span)
        span = requested;
    }
  size_t width
      = render_slice (NULL, context->source + start + offset, span, padding)
        - padding;
  fputs ("  ", stderr);
  for (size_t i = 0; i < padding; ++i)
    fputc (' ', stderr);
  fputc ('^', stderr);
  for (size_t i = 1; i < width; ++i)
    fputc ('~', stderr);
  fputc ('\n', stderr);
}

bool
parser_delimiter (ParserContext *context, SourceLocation location,
                  char delimiter)
{
  // Preserve only delimiter types: the grammar still builds the same AST for
  // either spelling. The stack belongs to this parse, including on failure.
  if (delimiter == '(' || delimiter == '[')
    {
      if (context->delimiter_count == context->delimiter_capacity)
        {
          context->delimiter_capacity = context->delimiter_capacity
                                            ? context->delimiter_capacity * 2
                                            : 32;
          context->delimiters
              = xrealloc (context->delimiters, context->delimiter_capacity);
        }
      context->delimiters[context->delimiter_count++] = delimiter;
      return true;
    }

  char message[80];
  if (context->delimiter_count == 0)
    snprintf (message, sizeof (message), "syntax error, unexpected '%c'",
              delimiter);
  else
    {
      char opening  = context->delimiters[context->delimiter_count - 1];
      char expected = opening == '[' ? ']' : ')';
      if (delimiter == expected)
        {
          --context->delimiter_count;
          return true;
        }
      snprintf (message, sizeof (message),
                "Mismatched delimiter '%c', expecting '%c'", delimiter,
                expected);
    }
  parser_report_error (context, location, message);
  return false;
}

static bool
parse_source (const char *name, const char *source, size_t length,
              Cell **ast_head, LM *lm)
{
  *ast_head = NIL;
  if (length > PARSER_MAX_SOURCE)
    {
      fprintf (stderr, "%s: error: source exceeds the scanner's size limit\n",
               name);
      return false;
    }
  ParserContext context = {
    .name = name, .source = source, .length = length, .line = 1, .column = 1
  };
  parser_at_eof (&context, &yylloc);
  yy_scan_bytes (source, (int)length);
  int status = yyparse (ast_head, lm, &context);
  yylex_destroy ();
  free (context.delimiters);
  if (status != 0 || context.has_error)
    {
      *ast_head = NIL;
      return false;
    }
  return true;
}

bool
parser_buf_named (const char *source_name, const char *input, Cell **ast_head,
                  LM *lm)
{
  *ast_head = NIL;
  if (!input)
    return false;
  return parse_source (source_name ? source_name : "<input>", input,
                       strlen (input), ast_head, lm);
}

bool
parser_buf (const char *input, Cell **ast_head, LM *lm)
{
  return parser_buf_named ("<input>", input, ast_head, lm);
}

bool
parser_stream_named (const char *source_name, FILE *input, Cell **ast_head,
                     LM *lm)
{
  *ast_head = NIL;
  if (!input)
    return false;
  const char *name     = source_name ? source_name : "<stream>";
  size_t      capacity = 4096;
  size_t      length   = 0;
  char       *source   = xmalloc (capacity);
  bool        readable = true;
  for (;;)
    {
      size_t count = fread (source + length, 1, capacity - length, input);
      length += count;
      if (ferror (input))
        {
          fprintf (stderr, "%s: error: unable to read source\n", name);
          readable = false;
          break;
        }
      if (count == 0)
        break;
      if (length == capacity)
        {
          if (capacity == PARSER_MAX_SOURCE)
            {
              // An exactly-full source is valid if the next read is EOF.
              int extra = fgetc (input);
              if (ferror (input))
                {
                  fprintf (stderr, "%s: error: unable to read source\n", name);
                  readable = false;
                }
              else if (extra != EOF)
                {
                  fprintf (
                      stderr,
                      "%s: error: source exceeds the scanner's size limit\n",
                      name);
                  readable = false;
                }
              break;
            }
          capacity = capacity > PARSER_MAX_SOURCE / 2 ? PARSER_MAX_SOURCE
                                                      : capacity * 2;
          source   = xrealloc (source, capacity);
        }
    }
  bool success = readable && parse_source (name, source, length, ast_head, lm);
  free (source);
  return success;
}

bool
parser_stream (FILE *input, Cell **ast_head, LM *lm)
{
  return parser_stream_named ("<stream>", input, ast_head, lm);
}
