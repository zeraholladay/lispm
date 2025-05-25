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
run_eval_program (const char *input)
{
  yyin = fmemopen ((void *)input, strlen (input), "r");

  int parse_status = yyparse (&ctx);
  ck_assert_int_eq (parse_status, 0);

  Node *program = CTX_PARSE_ROOT (&ctx);
  Node *eval_result = eval_program (program, &ctx);

  yylex_destroy ();
  fclose (yyin);

  return eval_result;
}

// literals

START_TEST (test_literal_expressions)
{
  Node *eval_result = NULL;

  eval_result = run_eval_program ("42");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_program ("-42");
  ck_assert (GET_INTEGER (eval_result) == -42);

  eval_result = run_eval_program ("T");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "T");

  eval_result = run_eval_program ("NIL");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("'foo");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "foo");
}
END_TEST

START_TEST (test_quote)
{
  Node *eval_result = NULL;
  Node *car = NULL;
  Node *cdr = NULL;

  // NULL program
  eval_result = run_eval_program ("()");
  ck_assert (eval_result);

  eval_result = run_eval_program ("'(foo)");
  ck_assert (!IS_NIL (eval_result));
  car = FIRST (eval_result);
  cdr = REST (eval_result);
  ck_assert_str_eq (GET_SYMBOL (car).str, "foo");
  ck_assert (IS_NIL (cdr));

  eval_result = run_eval_program ("'(foo bar)");
  ck_assert (!IS_NIL (eval_result));
  car = FIRST (eval_result);
  cdr = REST (eval_result);
  ck_assert (!IS_NIL (cdr));
  ck_assert_str_eq (GET_SYMBOL (car).str, "foo");
  ck_assert_str_eq (GET_SYMBOL (FIRST (cdr)).str, "bar");
}
END_TEST

START_TEST (test_cons)
{
  Node *eval_result = NULL;
  Node *car = NULL;
  Node *cdr = NULL;

  eval_result = run_eval_program ("(cons 'foo 'bar)");
  ck_assert (!IS_NIL (eval_result));
  car = FIRST (eval_result);
  cdr = REST (eval_result);
  ck_assert_str_eq (GET_SYMBOL (car).str, "foo");
  ck_assert_str_eq (GET_SYMBOL (cdr).str, "bar");

  eval_result = run_eval_program ("(cons 'foo '(bar))");
  ck_assert (!IS_NIL (eval_result));
  car = FIRST (eval_result);
  cdr = REST (eval_result);
  ck_assert (!IS_NIL (cdr));
  ck_assert_str_eq (GET_SYMBOL (FIRST (cdr)).str, "bar");
}
END_TEST

START_TEST (test_set_and_lookup)
{
  Node *eval_result = NULL;

  eval_result = run_eval_program ("(set 'foo 42)");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_program ("foo");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_program ("(set 'bar 'foo)");
  ck_assert (IS_SYMBOL (eval_result));

  eval_result = run_eval_program ("(set 'bar '(1 2 3))");
  ck_assert (IS_LIST (eval_result));

  eval_result = run_eval_program ("(set 'bar (lambda () ()))");
  ck_assert (IS_LAMBDA (eval_result));
}
END_TEST

START_TEST (test_first)
{
  Node *eval_result = NULL;

  eval_result = run_eval_program ("(first '())");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(first '(foo bar))");
  ck_assert (IS_SYMBOL (eval_result));
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "foo");

  if (setjmp (eval_error_jmp) == 0)
    {
      eval_result = run_eval_program ("(first)");
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

  eval_result = run_eval_program ("(rest '())");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(rest '(foo bar))");
  ck_assert (IS_LIST (eval_result));
  ck_assert (IS_SYMBOL (FIRST (eval_result)));
  ck_assert_str_eq (GET_SYMBOL (FIRST (eval_result)).str, "bar");

  if (setjmp (eval_error_jmp) == 0)
    {
      eval_result = run_eval_program ("(rest)");
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

  eval_result = run_eval_program ("(len '())");
  ck_assert (GET_INTEGER (eval_result) == 0);

  eval_result = run_eval_program ("(len '(a))");
  ck_assert (GET_INTEGER (eval_result) == 1);

  eval_result = run_eval_program ("(len '(a b))");
  ck_assert (GET_INTEGER (eval_result) == 2);
}
END_TEST

START_TEST (test_pair)
{
  Node *eval_result = NULL;

  eval_result = run_eval_program ("(pair '() '())");
  ck_assert (GET_INTEGER (eval_result) == 0);

  eval_result = run_eval_program ("(len '(a))");
  ck_assert (GET_INTEGER (eval_result) == 1);

  eval_result = run_eval_program ("(len '(a b))");
  ck_assert (GET_INTEGER (eval_result) == 2);
}
END_TEST

START_TEST (test_if)
{
  Node *eval_result = NULL;

  eval_result = run_eval_program ("(if T 1 2)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 1);

  eval_result = run_eval_program ("(if NIL 1 2)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 2);

  eval_result = run_eval_program ("(if T 42)");
  ck_assert_int_eq (GET_INTEGER (eval_result), 42);

  eval_result = run_eval_program ("(if NIL 42)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(if (< 2 3) 'yes 'no)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "yes");
}
END_TEST

START_TEST (test_list)
{
  Node *eval_result = NULL;

  eval_result = run_eval_program ("(list)");
  ck_assert (IS_NIL (eval_result));

  eval_result = run_eval_program ("(list 7)");
  ck_assert_int_eq (GET_INTEGER (FIRST (eval_result)), 7);
  ck_assert (IS_NIL (REST (eval_result)));

  eval_result = run_eval_program ("(list 1 2 3)");
  ck_assert_int_eq (GET_INTEGER (FIRST (eval_result)), 1);
  ck_assert_int_eq (GET_INTEGER (FIRST (REST (eval_result))), 2);
  ck_assert_int_eq (GET_INTEGER (FIRST (REST (REST (eval_result)))), 3);
  ck_assert (IS_NIL (REST (REST (REST (eval_result)))));

  eval_result = run_eval_program ("(list 'a 'b)");
  Node *second = FIRST (REST (eval_result));
  ck_assert_str_eq (GET_SYMBOL (FIRST (eval_result)).str, "a");
  ck_assert_str_eq (GET_SYMBOL (second).str, "b");
  ck_assert (IS_NIL (REST (REST (eval_result))));
}
END_TEST

START_TEST (test_lambda)
{
  Node *eval_result = NULL;

  // define
  eval_result = run_eval_program ("(lambda () ())");
  ck_assert (IS_LAMBDA (eval_result));

  // run
  eval_result = run_eval_program ("((lambda () ()))");
  ck_assert (IS_NIL (eval_result));

  // run
  eval_result = run_eval_program ("((lambda () (cons 'a 'b) 42))");
  ck_assert (IS_INTEGER (eval_result) && GET_INTEGER (eval_result) == 42);

  // run body with a=42
  eval_result = run_eval_program ("((lambda (a) a) 42)");
  ck_assert (IS_INTEGER (eval_result) && GET_INTEGER (eval_result) == 42);

  // define 'foo and run
  eval_result = run_eval_program ("(set 'foo (lambda () (cons 'a 'b)))"
                                  "(foo)");
  ck_assert (IS_LIST (eval_result));

  // with parameters
  eval_result = run_eval_program ("(set 'foo (lambda (a b) (cons a b)))"
                                  "(foo 'bar 'biz)");
  ck_assert (IS_LIST (eval_result));
  ck_assert_str_eq (GET_SYMBOL (FIRST (eval_result)).str, "bar");

  // test lexical scope
  eval_result = run_eval_program ("(set 'foo 'bar)"
                                  "((lambda () foo))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "bar");
}
END_TEST

START_TEST (test_apply)
{
  Node *eval_result = NULL;
  Node *car = NULL;
  Node *cdr = NULL;

  eval_result = run_eval_program ("(apply (lambda () ()) '())");
  ck_assert (IS_NIL (eval_result));

  eval_result
      = run_eval_program ("(apply (lambda (a b) (cons a b)) '(foo bar))");
  ck_assert (!IS_NIL (eval_result));
  car = FIRST (eval_result);
  cdr = REST (eval_result);
  ck_assert_str_eq (GET_SYMBOL (car).str, "foo");
  ck_assert_str_eq (GET_SYMBOL (cdr).str, "bar");

  eval_result = run_eval_program ("(apply set '(a 42))");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_program ("(apply first '((a 42)))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "a");

  eval_result = run_eval_program ("(first (apply rest '((a 42))))");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_program ("(first (apply cons '(a 42)))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "a");

  eval_result = run_eval_program ("(apply funcall '(first (42 2)))");
  ck_assert (GET_INTEGER (eval_result) == 42);
}
END_TEST

START_TEST (test_funcall)
{
  Node *eval_result = NULL;
  Node *car = NULL;
  Node *cdr = NULL;

  eval_result = run_eval_program ("(funcall (lambda () ()))");
  ck_assert (IS_NIL (eval_result));

  eval_result
      = run_eval_program ("(funcall (lambda (a b) (cons a b)) 'foo 'bar)");
  ck_assert (!IS_NIL (eval_result));
  car = FIRST (eval_result);
  cdr = REST (eval_result);
  ck_assert_str_eq (GET_SYMBOL (car).str, "foo");
  ck_assert_str_eq (GET_SYMBOL (cdr).str, "bar");

  eval_result = run_eval_program ("(funcall set 'a 42)");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_program ("(funcall first '(a 42))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "a");

  eval_result = run_eval_program ("(first (funcall rest '(a 42)))");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_program ("(first (funcall cons 'a 42))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "a");

  eval_result = run_eval_program ("(funcall apply 'first '((42 2)))");
  ck_assert (GET_INTEGER (eval_result) == 42);
}
END_TEST

START_TEST (test_eval)
{
  Node *eval_result = NULL;

  eval_result = run_eval_program ("(eval 42)");
  ck_assert (GET_INTEGER (eval_result) == 42);

  eval_result = run_eval_program ("(eval ''foobar)");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "foobar");

  eval_result = run_eval_program ("(eval '(first '(foo bar biz)))");
  ck_assert_str_eq (GET_SYMBOL (eval_result).str, "foo");
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

  suite_add_tcase (s, tc_core);
  return s;
}
