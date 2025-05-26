#include <check.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "parser.h"
#include "repl.h"
#include "types.h"

extern FILE *yyin;
extern int yyparse (Context *ctx);
extern void yylex_destroy (void);

extern void lispm_init (Context *ctx);
extern void lispm_destroy (Context *ctx);

static Context ctx = {};

jmp_buf eval_error_jmp;

static void
setup (void)
{
  lispm_init (&ctx);
}

static void
teardown (void)
{
  lispm_destroy (&ctx);
}

static Node *
run_eval_progn (const char *input)
{
  yyin = fmemopen ((void *)input, strlen (input), "r");

  int parse_status = yyparse (&ctx);
  ck_assert_int_eq (parse_status, 0);

  Node *program = CTX_PARSE_ROOT (&ctx);
  Node *eval_result = eval_progn (program, &ctx);

  yylex_destroy ();
  fclose (yyin);

  return eval_result;
}

// should test based on https://jtra.cz/stuff/lisp/sclr/index.html

// literals

START_TEST (test_literal_expressions)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("42");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_progn ("-42");
  ck_assert (GET_INTEGER (eval_result) == -42);

  eval_result = run_eval_progn ("T");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_progn ("NIL");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("'foo");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "foo");
}
END_TEST

START_TEST (test_quote)
{
  Node *eval_result = NULL;
  Node *car = NULL;
  Node *cdr = NULL;

  // NULL program
  eval_result = run_eval_progn ("()");
  ck_assert (eval_result);

  eval_result = run_eval_progn ("'(foo)");
  ck_assert (!IS_NIL (eval_result));
  car = CAR (eval_result);
  cdr = CDR (eval_result);
  ck_assert_str_eq (GET_SYMBOL (car).str, "foo");
  ck_assert (IS_NIL (cdr));

  eval_result = run_eval_progn ("'(foo bar)");
  ck_assert (!IS_NIL (eval_result));
  car = CAR (eval_result);
  cdr = CDR (eval_result);
  ck_assert (!IS_NIL (cdr));
  ck_assert_str_eq (GET_SYMBOL (car).str, "foo");
  ck_assert_str_eq (GET_SYMBOL (CAR (cdr)).str, "bar");
}
END_TEST

START_TEST (test_cons)
{
  Node *eval_result = NULL;
  Node *car = NULL;
  Node *cdr = NULL;

  eval_result = run_eval_progn ("(cons 'foo 'bar)");
  ck_assert (!IS_NIL (eval_result));
  car = CAR (eval_result);
  cdr = CDR (eval_result);
  ck_assert_str_eq (GET_SYMBOL (car).str, "foo");
  ck_assert_str_eq (GET_SYMBOL (cdr).str, "bar");

  eval_result = run_eval_progn ("(cons 'foo '(bar))");
  ck_assert (!IS_NIL (eval_result));
  car = CAR (eval_result);
  cdr = CDR (eval_result);
  ck_assert (!IS_NIL (cdr));
  ck_assert_str_eq (GET_SYMBOL (CAR (cdr)).str, "bar");
}
END_TEST

START_TEST (test_set_and_lookup)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(set 'foo 42)");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_progn ("foo");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_progn ("(set 'bar 'foo)");
  ck_assert (IS_SYMBOL (eval_result));

  eval_result = run_eval_progn ("(set 'bar '(1 2 3))");
  ck_assert (IS_CONS (eval_result));

  eval_result = run_eval_progn ("(set 'bar (lambda () ()))");
  ck_assert (IS_LAMBDA (eval_result));
}
END_TEST

START_TEST (test_first)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(first '())");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(first '(foo bar))");
  ck_assert (IS_SYMBOL (eval_result));
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "foo");

  if (setjmp (eval_error_jmp) == 0)
    {
      eval_result = run_eval_progn ("(first)");
      ck_assert (0);
    }
  else
    {
      ck_assert (1);
    }
}
END_TEST

START_TEST (test_rest)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(rest '())");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(rest '(foo bar))");
  ck_assert (IS_CONS (eval_result));
  ck_assert (IS_SYMBOL (CAR (eval_result)));
  ck_assert_str_eq (GET_SYMBOL (CAR (eval_result)).str, "bar");

  if (setjmp (eval_error_jmp) == 0)
    {
      eval_result = run_eval_progn ("(rest)");
      ck_assert (0);
    }
  else
    {
      ck_assert (1);
    }
}
END_TEST

START_TEST (test_len)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(len '())");
  ck_assert (GET_INTEGER (eval_result) == 0);

  eval_result = run_eval_progn ("(len '(a))");
  ck_assert (GET_INTEGER (eval_result) == 1);

  eval_result = run_eval_progn ("(len '(a b))");
  ck_assert (GET_INTEGER (eval_result) == 2);
}
END_TEST

START_TEST (test_pair)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(len (pair '() '()))");
  ck_assert (GET_INTEGER (eval_result) == 0);

  eval_result = run_eval_progn ("(len '(a))");
  ck_assert (GET_INTEGER (eval_result) == 1);

  eval_result = run_eval_progn ("(len '(a b))");
  ck_assert (GET_INTEGER (eval_result) == 2);
}
END_TEST

START_TEST (test_if)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(if T 1 2)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 1);

  eval_result = run_eval_progn ("(if NIL 1 2)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 2);

  eval_result = run_eval_progn ("(if T 42)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 42);

  eval_result = run_eval_progn ("(if NIL 42)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(if (< 2 3) 'yes 'no)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "yes");
}
END_TEST

START_TEST (test_list)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(list)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_progn ("(list 7)");
  ck_assert_int_eq (GET_INTEGER (CAR (eval_result)), 7);
  ck_assert (IS_NIL (CDR (eval_result)));

  eval_result = run_eval_progn ("(list 1 2 3)");
  ck_assert_int_eq (GET_INTEGER (CAR (eval_result)), 1);
  ck_assert_int_eq (GET_INTEGER (CAR (CDR (eval_result))), 2);
  ck_assert_int_eq (GET_INTEGER (CAR (CDR (CDR (eval_result)))), 3);
  ck_assert (IS_NIL (CDR (CDR (CDR (eval_result)))));

  eval_result = run_eval_progn ("(list 'a 'b)");
  Node *second = CAR (CDR (eval_result));
  ck_assert_str_eq (GET_SYMBOL (CAR (eval_result)).str, "a");
  ck_assert_str_eq (GET_SYMBOL (second).str, "b");
  ck_assert (IS_NIL (CDR (CDR (eval_result))));
}
END_TEST

START_TEST (test_lambda)
{
  Node *eval_result = NULL;

  // define
  eval_result = run_eval_progn ("(lambda () ())");
  ck_assert (IS_LAMBDA (eval_result));

  // run
  eval_result = run_eval_progn ("((lambda () ()))");
  ck_assert (IS_NIL (eval_result));

  // run
  eval_result = run_eval_progn ("((lambda () (cons 'a 'b) 42))");
  ck_assert (IS_INTEGER (eval_result) && GET_INTEGER (eval_result) == 42);

  // run body with a=42
  eval_result = run_eval_progn ("((lambda (a) a) 42)");
  ck_assert (IS_INTEGER (eval_result) && GET_INTEGER (eval_result) == 42);

  // define 'foo and run
  eval_result = run_eval_progn ("(set 'foo (lambda () (cons 'a 'b)))"
                                "(foo)");
  ck_assert (IS_CONS (eval_result));

  // with parameters
  eval_result = run_eval_progn ("(set 'foo (lambda (a b) (cons a b)))"
                                "(foo 'bar 'biz)");
  ck_assert (IS_CONS (eval_result));
  ck_assert_str_eq (GET_SYMBOL (CAR (eval_result)).str, "bar");

  // test lexical scope
  eval_result = run_eval_progn ("(set 'foo 'bar)"
                                "((lambda () foo))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "bar");
}
END_TEST

START_TEST (test_apply)
{
  Node *eval_result = NULL;
  Node *car = NULL;
  Node *cdr = NULL;

  eval_result = run_eval_progn ("(apply (lambda () ()) '())");
  ck_assert (IS_NIL (eval_result));

  // (apply #'+ 1 2 3 '(4 5 6))

  eval_result
      = run_eval_progn ("(apply (lambda (a b) (cons a b)) '(foo bar))");
  ck_assert (!IS_NIL (eval_result));
  car = CAR (eval_result);
  cdr = CDR (eval_result);
  ck_assert_str_eq (GET_SYMBOL (car).str, "foo");
  ck_assert_str_eq (GET_SYMBOL (cdr).str, "bar");

  eval_result = run_eval_progn ("(apply set '(a 42))");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_progn ("(apply first '((a 42)))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "a");

  eval_result = run_eval_progn ("(first (apply rest '((a 42))))");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_progn ("(first (apply cons '(a 42)))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "a");

  eval_result = run_eval_progn ("(apply funcall '(first (42 2)))");
  ck_assert (GET_INTEGER (eval_result) == 42);
}
END_TEST

START_TEST (test_funcall)
{
  Node *eval_result = NULL;
  Node *car = NULL;
  Node *cdr = NULL;

  eval_result = run_eval_progn ("(funcall (lambda () ()))");
  ck_assert (IS_NIL (eval_result));

  eval_result
      = run_eval_progn ("(funcall (lambda (a b) (cons a b)) 'foo 'bar)");
  ck_assert (!IS_NIL (eval_result));
  car = CAR (eval_result);
  cdr = CDR (eval_result);
  ck_assert_str_eq (GET_SYMBOL (car).str, "foo");
  ck_assert_str_eq (GET_SYMBOL (cdr).str, "bar");

  eval_result = run_eval_progn ("(funcall set 'a 42)");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_progn ("(funcall first '(a 42))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "a");

  eval_result = run_eval_progn ("(first (funcall rest '(a 42)))");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_progn ("(first (funcall cons 'a 42))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "a");

  eval_result = run_eval_progn ("(funcall apply 'first '((42 2)))");
  ck_assert (GET_INTEGER (eval_result) == 42);
}
END_TEST

START_TEST (test_eval)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(eval 42)");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_progn ("(eval ''foobar)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "foobar");

  eval_result = run_eval_progn ("(eval '(first '(foo bar biz)))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "foo");
}
END_TEST

START_TEST (test_last)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(last '(1 2 3 42))");
  ck_assert (GET_INTEGER (eval_result) == 42);
}
END_TEST

START_TEST (test_butlast)
{
  Node *eval_result = NULL;

  eval_result = run_eval_progn ("(apply + (BUTLAST '(1 2 3 4)))");
  ck_assert (GET_INTEGER (eval_result) == 6);
}
END_TEST

// (mapcar (lambda (x) (+ x 10)) '(1 2 3 4)) => (11 12 13 14)
// (mapcar #'round '(1.3 2.7 3.4 4.5)) => (1 3 3 4)
// (mapcar #'list '(123 symbol "string" 345) '(1 2 3)) => ((123 1) (SYMBOL 2)
// ("string" 3)) (mapcar #'* '(3 4 5) '(4 5 6)) => (12 20 30)
START_TEST (test_mapcar)
{
  Node *eval_result = NULL;

  eval_result
      = run_eval_progn ("(apply + (mapcar (lambda (x) (+ x 10)) '(1 2 3 4)))");
  ck_assert (GET_INTEGER (eval_result) == 50);

  eval_result = run_eval_progn ("(apply + (mapcar * '(3 4 5) '(4 5 6)))");
  ck_assert (GET_INTEGER (eval_result) == 62);
}
END_TEST

Suite *
eval_suite (void)
{
  Suite *s = suite_create ("Eval");

  TCase *tc_core = tcase_create ("Core");
  tcase_add_checked_fixture (tc_core, setup, teardown);

  tcase_add_test (tc_core, test_literal_expressions);
  tcase_add_test (tc_core, test_quote);
  tcase_add_test (tc_core, test_cons);
  tcase_add_test (tc_core, test_set_and_lookup);
  tcase_add_test (tc_core, test_first);
  tcase_add_test (tc_core, test_rest);
  tcase_add_test (tc_core, test_len);
  tcase_add_test (tc_core, test_pair);
  tcase_add_test (tc_core, test_if);
  tcase_add_test (tc_core, test_list);
  tcase_add_test (tc_core, test_lambda);
  tcase_add_test (tc_core, test_apply);
  tcase_add_test (tc_core, test_funcall);
  tcase_add_test (tc_core, test_eval);
  tcase_add_test (tc_core, test_last);
  tcase_add_test (tc_core, test_butlast);
  tcase_add_test (tc_core, test_mapcar);

  suite_add_tcase (s, tc_core);
  return s;
}
