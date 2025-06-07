#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "format.h"
#include "lispm.h"
#include "parser.h"
#include "repl.h"
#include "types.h"

static Cell *progn = NULL;
static LM *lm = NULL;

static void
setup (void)
{
  lm = lm_create ();
}

static void
teardown (void)
{
  lm_destroy (lm);
}

static Cell *
run_eval_progn (const char *input)
{
  ck_assert (parser_buf (input, &progn, lm));
  return lm_progn (lm, progn);
}

// should test based on https://jtra.cz/stuff/lisp/sclr/index.html

// literals

START_TEST (test_literal_expressions)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("42");
  ck_assert (eval_res->integer == 42);

  eval_res = run_eval_progn ("-42");
  ck_assert (eval_res->integer == -42);

  eval_res = run_eval_progn ("T");
  ck_assert_str_eq (eval_res->symbol.str, "T");

  eval_res = run_eval_progn ("NIL");
  ck_assert (IS_NIL (eval_res));

  eval_res = run_eval_progn ("'foo");
  ck_assert_str_eq (eval_res->symbol.str, "foo");
}
END_TEST

START_TEST (test_quote)
{
  Cell *eval_res = NULL;
  Cell *car = NULL;
  Cell *cdr = NULL;

  // NULL progn
  eval_res = run_eval_progn ("()");
  ck_assert (eval_res);

  eval_res = run_eval_progn ("'(foo)");
  ck_assert (!IS_NIL (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert_str_eq (car->symbol.str, "foo");
  ck_assert (IS_NIL (cdr));

  eval_res = run_eval_progn ("'(foo bar)");
  ck_assert (!IS_NIL (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert (!IS_NIL (cdr));
  ck_assert_str_eq (car->symbol.str, "foo");
  ck_assert_str_eq (CAR (cdr)->symbol.str, "bar");
}
END_TEST

START_TEST (test_cons)
{
  Cell *eval_res = NULL;
  Cell *car = NULL;
  Cell *cdr = NULL;

  eval_res = run_eval_progn ("(cons 'foo 'bar)");
  ck_assert (!IS_NIL (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert_str_eq (car->symbol.str, "foo");
  ck_assert_str_eq (cdr->symbol.str, "bar");

  eval_res = run_eval_progn ("(cons 'foo '(bar))");
  ck_assert (!IS_NIL (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert (!IS_NIL (cdr));
  ck_assert_str_eq (CAR (cdr)->symbol.str, "bar");
}
END_TEST

START_TEST (test_set_and_lookup)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(set 'foo 42)");
  ck_assert (eval_res->integer == 42);

  eval_res = run_eval_progn ("foo");
  ck_assert (eval_res->integer == 42);

  eval_res = run_eval_progn ("(set 'bar 'foo)");
  ck_assert (IS_INST (eval_res, SYMBOL));

  eval_res = run_eval_progn ("(set 'bar '(1 2 3))");
  ck_assert (IS_INST (eval_res, CONS));

  eval_res = run_eval_progn ("(set 'bar (lambda () ()))");
  ck_assert (IS_INST (eval_res, LAMBDA));
}
END_TEST

START_TEST (test_first)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(first '())");
  ck_assert (IS_NIL (eval_res));

  eval_res = run_eval_progn ("(first '(foo bar))");
  ck_assert (IS_INST (eval_res, SYMBOL));
  ck_assert_str_eq (eval_res->symbol.str, "foo");

  // eval_res = run_eval_progn ("(first)");
  // ck_assert (IS_INST (eval_res, ERROR));
}
END_TEST

START_TEST (test_rest)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(rest '())");
  ck_assert (IS_NIL (eval_res));

  eval_res = run_eval_progn ("(rest '(foo bar))");
  ck_assert (IS_INST (eval_res, CONS));
  ck_assert (IS_INST (CAR (eval_res), SYMBOL));
  ck_assert_str_eq (CAR (eval_res)->symbol.str, "bar");

  // eval_res = run_eval_progn ("(rest)");
  // ck_assert (IS_INST (eval_res, ERROR));
}
END_TEST

START_TEST (test_len)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(length '())");
  ck_assert (eval_res->integer == 0);

  eval_res = run_eval_progn ("(length '(a))");
  ck_assert (eval_res->integer == 1);

  eval_res = run_eval_progn ("(length '(a b))");
  ck_assert (eval_res->integer == 2);
}
END_TEST

START_TEST (test_if)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(if T 1 2)");
  ck_assert_int_eq (eval_res->integer, 1);

  eval_res = run_eval_progn ("(if NIL 1 2)");
  ck_assert_int_eq (eval_res->integer, 2);

  eval_res = run_eval_progn ("(if T 42)");
  ck_assert_int_eq (eval_res->integer, 42);

  eval_res = run_eval_progn ("(if NIL 42)");
  ck_assert (IS_NIL (eval_res));

  eval_res = run_eval_progn ("(if (< 2 3) 'yes 'no)");
  ck_assert_str_eq (eval_res->symbol.str, "yes");
}
END_TEST

START_TEST (test_list)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(list)");
  ck_assert (IS_NIL (eval_res));

  eval_res = run_eval_progn ("(list 7)");
  ck_assert_int_eq (CAR (eval_res)->integer, 7);
  ck_assert (IS_NIL (CDR (eval_res)));

  eval_res = run_eval_progn ("(list 1 2 3)");
  ck_assert_int_eq (CAR (eval_res)->integer, 1);
  ck_assert_int_eq (CAR (CDR (eval_res))->integer, 2);
  ck_assert_int_eq (CAR (CDR (CDR (eval_res)))->integer, 3);
  ck_assert (IS_NIL (CDR (CDR (CDR (eval_res)))));

  eval_res = run_eval_progn ("(list 'a 'b)");
  Cell *second = CAR (CDR (eval_res));
  ck_assert_str_eq (CAR (eval_res)->symbol.str, "a");
  ck_assert_str_eq (second->symbol.str, "b");
  ck_assert (IS_NIL (CDR (CDR (eval_res))));
}
END_TEST

START_TEST (test_lambda)
{
  Cell *eval_res = NULL;

  // define
  eval_res = run_eval_progn ("(lambda () ())");
  ck_assert (IS_INST (eval_res, LAMBDA));

  // run
  eval_res = run_eval_progn ("((lambda () ()))");
  ck_assert (IS_NIL (eval_res));

  // run
  eval_res = run_eval_progn ("((lambda () (cons 'a 'b) 42))");
  ck_assert (IS_INST (eval_res, INTEGER) && eval_res->integer == 42);

  // run body with a=42
  eval_res = run_eval_progn ("((lambda (a) a) 42)");
  ck_assert (IS_INST (eval_res, INTEGER) && eval_res->integer == 42);

  // define 'foo and run
  eval_res = run_eval_progn ("(set 'foo (lambda () (cons 'a 'b)))"
                             "(foo)");
  ck_assert (IS_INST (eval_res, CONS));

  // with parameters
  eval_res = run_eval_progn ("(set 'foo (lambda (a b) (cons a b)))"
                             "(foo 'bar 'biz)");
  ck_assert (IS_INST (eval_res, CONS));
  ck_assert_str_eq (CAR (eval_res)->symbol.str, "bar");

  // test lexical scope
  eval_res = run_eval_progn ("(set 'foo 'bar)"
                             "((lambda () foo))");
  ck_assert_str_eq (eval_res->symbol.str, "bar");
}
END_TEST

START_TEST (test_apply)
{
  Cell *eval_res = NULL;
  Cell *car = NULL;
  Cell *cdr = NULL;

  eval_res = run_eval_progn ("(apply (lambda () ()) '())");
  ck_assert (IS_NIL (eval_res));

  // (apply #'+ 1 2 3 '(4 5 6))

  eval_res = run_eval_progn ("(apply (lambda (a b) (cons a b)) '(foo bar))");
  ck_assert (!IS_NIL (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert_str_eq (car->symbol.str, "foo");
  ck_assert_str_eq (cdr->symbol.str, "bar");

  eval_res = run_eval_progn ("(apply set '(a 42))");
  ck_assert (eval_res->integer == 42);

  eval_res = run_eval_progn ("(apply first '((a 42)))");
  ck_assert_str_eq (eval_res->symbol.str, "a");

  eval_res = run_eval_progn ("(first (apply rest '((a 42))))");
  ck_assert (eval_res->integer == 42);

  eval_res = run_eval_progn ("(first (apply cons '(a 42)))");
  ck_assert_str_eq (eval_res->symbol.str, "a");

  // eval_res = run_eval_progn ("(apply funcall '(first (42 2)))");
  // ck_assert (eval_res->integer == 42);
}
END_TEST

START_TEST (test_funcall)
{
  Cell *eval_res = NULL;
  Cell *car = NULL;
  Cell *cdr = NULL;

  eval_res = run_eval_progn ("(funcall (lambda () ()))");
  ck_assert (IS_NIL (eval_res));

  eval_res = run_eval_progn ("(funcall (lambda (a b) (cons a b)) 'foo 'bar)");
  ck_assert (!IS_NIL (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert_str_eq (car->symbol.str, "foo");
  ck_assert_str_eq (cdr->symbol.str, "bar");

  eval_res = run_eval_progn ("(funcall set 'a 42)");
  ck_assert (eval_res->integer == 42);

  eval_res = run_eval_progn ("(funcall first '(a 42))");
  ck_assert_str_eq (eval_res->symbol.str, "a");

  eval_res = run_eval_progn ("(first (funcall rest '(a 42)))");
  ck_assert (eval_res->integer == 42);

  eval_res = run_eval_progn ("(first (funcall cons 'a 42))");
  ck_assert_str_eq (eval_res->symbol.str, "a");

  // eval_res = run_eval_progn ("(funcall apply 'first '((42 2)))");
  // ck_assert (eval_res->integer == 42);
}
END_TEST

START_TEST (test_eval)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(eval 42)");
  ck_assert (eval_res->integer == 42);

  eval_res = run_eval_progn ("(eval ''foobar)");
  ck_assert_str_eq (eval_res->symbol.str, "foobar");

  eval_res = run_eval_progn ("(eval '(first '(foo bar biz)))");
  ck_assert_str_eq (eval_res->symbol.str, "foo");
}
END_TEST

START_TEST (test_last)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(last '(1 2 3 42))");
  ck_assert (CAR (eval_res)->integer == 42);
}
END_TEST

START_TEST (test_butlast)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(apply + (BUTLAST '(1 2 3 4)))");
  ck_assert (eval_res->integer == 6);
}
END_TEST

// (mapcar (lambda (x) (+ x 10)) '(1 2 3 4)) => (11 12 13 14)
// (mapcar #'round '(1.3 2.7 3.4 4.5)) => (1 3 3 4)
// (mapcar #'list '(123 symbol "string" 345) '(1 2 3)) => ((123 1) (SYMBOL 2)
// ("string" 3)) (mapcar #'* '(3 4 5) '(4 5 6)) => (12 20 30)
START_TEST (test_mapcar)
{
  Cell *eval_res = NULL;

  eval_res
      = run_eval_progn ("(apply + (mapcar (lambda (x) (+ x 10)) '(1 2 3 4)))");
  ck_assert (eval_res->integer == 50);

  eval_res = run_eval_progn ("(apply + (mapcar * '(3 4 5) '(4 5 6)))");
  ck_assert (eval_res->integer == 62);
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
