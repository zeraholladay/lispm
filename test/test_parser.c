#include <check.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "lm.h"
#include "parser.h"
#include "prims.h"

static LM   *lm;
static Cell *parsed;
static char  diagnostic[16384];

static void
setup (void)
{
  lm = lm_create ();
  ck_assert_ptr_nonnull (lm);
}

static void
teardown (void)
{
  lm_destroy (lm);
}

// Capture actual user-facing output, restoring stderr before any assertions
// inspect it. Passing a length lets stream tests include non-terminated bytes.
static bool
capture_parse (const char *name, const char *source, size_t length,
               bool stream)
{
  FILE *log = tmpfile ();
  ck_assert_ptr_nonnull (log);
  FILE *input = NULL;
  if (stream)
    {
      input = tmpfile ();
      ck_assert_ptr_nonnull (input);
      ck_assert_uint_eq (fwrite (source, 1, length, input), length);
      rewind (input);
    }
  fflush (stderr);
  int saved = dup (STDERR_FILENO);
  ck_assert_int_ge (saved, 0);
  ck_assert_int_ge (dup2 (fileno (log), STDERR_FILENO), 0);

  parsed       = T;
  bool success = stream ? parser_stream_named (name, input, &parsed, lm)
                        : parser_buf_named (name, source, &parsed, lm);
  fflush (stderr);
  int restored = dup2 (saved, STDERR_FILENO);
  close (saved);
  ck_assert_int_ge (restored, 0);
  if (input)
    fclose (input);
  rewind (log);
  size_t count      = fread (diagnostic, 1, sizeof (diagnostic) - 1, log);
  diagnostic[count] = '\0';
  ck_assert (!ferror (log));
  ck_assert (feof (log));
  fclose (log);
  if (!success)
    ck_assert_ptr_eq (parsed, NIL);
  return success;
}

static bool
parse_buffer (const char *name, const char *source)
{
  return capture_parse (name, source, strlen (source), false);
}

static bool
parse_stream (const char *name, const char *source)
{
  return capture_parse (name, source, strlen (source), true);
}

static void
assert_single_diagnostic (void)
{
  const char *first = strstr (diagnostic, ": error:");
  ck_assert_ptr_nonnull (first);
  ck_assert_ptr_null (strstr (first + 1, ": error:"));
}

START_TEST (test_multiline_unexpected_token)
{
  ck_assert (
      !parse_buffer ("sample.scm", "; heading\n(define answer 42)\n  )"));
  ck_assert_ptr_nonnull (strstr (
      diagnostic, "sample.scm:3:3: error: syntax error, unexpected ')'"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "    )\n    ^\n"));
  assert_single_diagnostic ();
}
END_TEST

START_TEST (test_unknown_character_and_tab_alignment)
{
  ck_assert (!parse_buffer ("tabbed.scm", "; comment\n\t@"));
  ck_assert_ptr_nonnull (
      strstr (diagnostic, "tabbed.scm:2:2: error: Unexpected character '@'"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "          @\n          ^\n"));
  ck_assert_ptr_null (strchr (diagnostic, '\t'));
  assert_single_diagnostic ();
}
END_TEST

START_TEST (test_eof_locations)
{
  ck_assert (!parse_buffer ("eof.scm", "(\n  + 1 2"));
  ck_assert_ptr_nonnull (strstr (
      diagnostic, "eof.scm:2:8: error: syntax error, unexpected end of file"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "    + 1 2\n         ^\n"));
  assert_single_diagnostic ();

  ck_assert (!parse_buffer ("eof.scm", "(\n"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "eof.scm:2:1: error:"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "\n  \n  ^\n"));
}
END_TEST

START_TEST (test_detailed_readable_token_names)
{
  ck_assert (!parse_buffer ("details.scm", "(lambda 42 x)"));
  ck_assert_ptr_nonnull (strstr (
      diagnostic, "details.scm:1:9: error: syntax error, unexpected integer"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "expecting"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "identifier"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "^~\n"));
}
END_TEST

START_TEST (test_parameter_error_uses_parameter_location)
{
  ck_assert (!parse_buffer ("params.scm", "(lambda (x x)\n  x)"));
  ck_assert_ptr_nonnull (
      strstr (diagnostic, "params.scm:1:9: error: Lambda parameters"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "          ^~~~~\n"));

  ck_assert (!parse_buffer ("params.scm", "(define (f x . x)\n  x)"));
  ck_assert_ptr_nonnull (
      strstr (diagnostic, "params.scm:1:12: error: Function parameters"));
  assert_single_diagnostic ();
}
END_TEST

START_TEST (test_numeric_errors_have_token_spans)
{
  ck_assert (!parse_buffer ("numbers.scm", " \n    -9223372036854775809"));
  ck_assert_ptr_nonnull (strstr (
      diagnostic, "numbers.scm:2:5: error: Integer literal out of range"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "^~~~~~~~~~~~~~~~~~~~\n"));
  assert_single_diagnostic ();

  // A numeric lexical error must retain its message even where the grammar
  // expects a parameter name, rather than reducing an ordinary numeric form.
  ck_assert (
      !parse_buffer ("numbers.scm", "(lambda (9223372036854775808) 1)"));
  ck_assert_ptr_nonnull (strstr (
      diagnostic, "numbers.scm:1:10: error: Integer literal out of range"));
  ck_assert_ptr_null (strstr (diagnostic, "syntax error"));
  assert_single_diagnostic ();

  char source[420] = "  ";
  memset (source + 2, '9', 400);
  memcpy (source + 402, ".0", 3);
  ck_assert (!parse_buffer ("numbers.scm", source));
  ck_assert_ptr_nonnull (
      strstr (diagnostic,
              "numbers.scm:1:3: error: Floating-point literal out of range"));
  assert_single_diagnostic ();

  // A number must end at a delimiter rather than splitting into two tokens.
  const char *glued[] = { "(+ 1-2)", "1e3", "'(1.5.5)", "3abc", "1." };
  for (size_t i = 0; i < sizeof (glued) / sizeof (glued[0]); ++i)
    {
      ck_assert (!parse_buffer ("numbers.scm", glued[i]));
      ck_assert_ptr_nonnull (strstr (diagnostic, "Invalid numeric literal"));
      assert_single_diagnostic ();
    }
  ck_assert (!parse_buffer ("numbers.scm", "(+ 1-2)"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "numbers.scm:1:4:"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "   ^~~\n"));
  ck_assert (parse_buffer ("numbers.scm", "(+ 1 -2) '(1 . 2.5)[3];x\n4"));
}
END_TEST

START_TEST (test_crlf_and_comments)
{
  ck_assert (!parse_stream (
      "windows.scm", "; heading\r\n(+ 1 2) ; trailing comment\r\n @\r\n"));
  ck_assert_ptr_nonnull (
      strstr (diagnostic, "windows.scm:3:2: error: Unexpected character '@'"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "   @\n   ^\n"));
  ck_assert_ptr_null (strchr (diagnostic, '\r'));
  assert_single_diagnostic ();
}
END_TEST

START_TEST (test_location_reset_across_buffers_and_streams)
{
  ck_assert (!parse_buffer ("first.scm", "\n\n)"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "first.scm:3:1:"));
  ck_assert (parse_stream ("valid.scm", "(+ 20 22)"));
  ck_assert_str_eq (diagnostic, "");
  EvaluationResult result = lm_evaluate (lm, parsed);
  ck_assert (result.success);
  ck_assert_int_eq (result.value->integer, 42);
  ck_assert (!parse_stream ("second.scm", " \n @"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "second.scm:2:2:"));
  ck_assert (!parse_buffer (NULL, "@"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "<input>:1:1:"));
  ck_assert (!parse_stream (NULL, "("));
  ck_assert_ptr_nonnull (strstr (diagnostic, "<stream>:1:2:"));
  ck_assert (parse_buffer ("empty.scm", "; just a comment\n"));
  ck_assert_ptr_eq (parsed, NIL);
  ck_assert_str_eq (diagnostic, "");
}
END_TEST

START_TEST (test_buffered_stream_boundaries_and_control_bytes)
{
  char source[5010];
  memset (source, 'x', 5000);
  source[0] = ';';
  memcpy (source + 5000, "\n )", 4);
  ck_assert (!parse_stream ("chunks.scm", source));
  ck_assert_ptr_nonnull (strstr (diagnostic, "chunks.scm:2:2: error:"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "   )\n   ^\n"));

  char binary[] = { '1', ' ', '\0', ')' };
  ck_assert (!capture_parse ("bytes.scm", binary, sizeof (binary), true));
  ck_assert_ptr_nonnull (
      strstr (diagnostic, "bytes.scm:1:3: error: Unexpected byte 0x00"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "  1 \\x00)\n    ^~~~\n"));
  assert_single_diagnostic ();
}
END_TEST

START_TEST (test_brackets_evaluate_like_parentheses)
{
  const char *sources[] = { "[+ 20 22]",
                            "(let ([x 20] [y 22]) [+ x y])",
                            "[[lambda [x y] [+ x y]] 20 22]",
                            "[define [f x . rest] [+ x (car rest)]] [f 20 22]",
                            "[car '[42 . ignored]]",
                            "[car '[42 (ignored)]]",
                            "[if [] 0 42]",
                            "[let [] [define x 20] [set! x [+ x 22]] x]",
                            "[+ 20 ; ignored delimiters: [ ) ] (\n 22]" };
  for (size_t i = 0; i < sizeof (sources) / sizeof (sources[0]); ++i)
    {
      ck_assert (capture_parse ("brackets.scm", sources[i],
                                strlen (sources[i]), i % 2));
      ck_assert_str_eq (diagnostic, "");
      EvaluationResult result = lm_evaluate (lm, parsed);
      ck_assert (result.success);
      ck_assert (cell_has_type (result.value, CELL_TYPE_INTEGER));
      ck_assert_int_eq (result.value->integer, 42);
    }
}
END_TEST

START_TEST (test_bracket_mismatches_and_recovery)
{
  ck_assert (!parse_buffer ("brackets.scm", "(a b]"));
  ck_assert_ptr_nonnull (strstr (
      diagnostic,
      "brackets.scm:1:5: error: Mismatched delimiter ']', expecting ')'"));
  ck_assert_ptr_nonnull (strstr (diagnostic, "  (a b]\n      ^\n"));
  assert_single_diagnostic ();
  ck_assert (!parse_stream ("brackets.scm", "[a (b] c)"));
  ck_assert_ptr_nonnull (strstr (
      diagnostic,
      "brackets.scm:1:6: error: Mismatched delimiter ']', expecting ')'"));
  assert_single_diagnostic ();
  ck_assert (!parse_buffer ("brackets.scm", "[a b)"));
  ck_assert_ptr_nonnull (strstr (
      diagnostic,
      "brackets.scm:1:5: error: Mismatched delimiter ')', expecting ']'"));
  assert_single_diagnostic ();
  ck_assert (!parse_buffer ("brackets.scm", "]"));
  ck_assert_ptr_nonnull (strstr (
      diagnostic, "brackets.scm:1:1: error: syntax error, unexpected ']'"));
  assert_single_diagnostic ();
  ck_assert (!parse_stream ("brackets.scm", "[\n  + 1 2"));
  ck_assert_ptr_nonnull (
      strstr (diagnostic, "brackets.scm:2:8: error: syntax error, unexpected "
                          "end of file, expecting ']'"));
  assert_single_diagnostic ();
  // Grammar errors and lexical errors also leave open delimiters behind.
  ck_assert (!parse_buffer ("brackets.scm", "[lambda 42 x]"));
  ck_assert (!parse_buffer ("brackets.scm", "[(@"));
  ck_assert (parse_buffer ("recovered.scm", "[+ 20 22]"));
  ck_assert_str_eq (diagnostic, "");
}
END_TEST

START_TEST (test_deep_mixed_delimiters)
{
  char source[194];
  for (size_t i = 0; i < 96; ++i)
    {
      source[i]       = i % 2 ? '[' : '(';
      source[192 - i] = i % 2 ? ']' : ')';
    }
  source[96]  = '0';
  source[193] = '\0';
  ck_assert (parse_stream ("nested.scm", source));
  ck_assert_str_eq (diagnostic, "");
}
END_TEST

Suite *
parser_suite (void)
{
  Suite *suite = suite_create ("Parser Diagnostics");
  TCase *tc    = tcase_create ("Locations");
  tcase_add_checked_fixture (tc, setup, teardown);
  tcase_add_test (tc, test_multiline_unexpected_token);
  tcase_add_test (tc, test_unknown_character_and_tab_alignment);
  tcase_add_test (tc, test_eof_locations);
  tcase_add_test (tc, test_detailed_readable_token_names);
  tcase_add_test (tc, test_parameter_error_uses_parameter_location);
  tcase_add_test (tc, test_numeric_errors_have_token_spans);
  tcase_add_test (tc, test_crlf_and_comments);
  tcase_add_test (tc, test_location_reset_across_buffers_and_streams);
  tcase_add_test (tc, test_buffered_stream_boundaries_and_control_bytes);
  tcase_add_test (tc, test_brackets_evaluate_like_parentheses);
  tcase_add_test (tc, test_bracket_mismatches_and_recovery);
  tcase_add_test (tc, test_deep_mixed_delimiters);
  suite_add_tcase (suite, tc);
  return suite;
}
