#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fmt.h"
#include "lm.h"
#include "lm_gc.h"
#include "parser.h"
#include "prims.h"

static LM *lm;

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

static Cell *
evaluate (const char *source)
{
  Cell *forms = NULL;
  ck_assert (parser_buf (source, &forms, lm));
  return lm_progn (lm, forms);
}

static void
expect (const char *source, const char *expected)
{
  char *actual = format (evaluate (source));
  ck_assert_msg (strcmp (actual, expected) == 0, "%s: expected %s, got %s",
                 source, expected, actual);
  free (actual);
}

START_TEST (test_rest_only_parameters)
{
  expect ("((lambda args args))", "nil");
  expect ("((lambda args args) 1)", "(1)");
  expect ("((lambda args args) 1 'a NIL)", "(1 a nil)");
  expect ("(define (collect . args) args) (collect)", "nil");
  expect ("(collect 1 2 3)", "(1 2 3)");
  expect ("(define collect2 (lambda args args)) (collect2 4 5)", "(4 5)");
}
END_TEST

START_TEST (test_required_and_rest_parameters)
{
  expect ("((lambda (head . extras) (cons head extras)) 1)", "(1)");
  expect ("((lambda (head . extras) (cons head extras)) 1 2 3)", "(1 2 3)");
  expect ("(define (split a b . extras) (list a b extras)) (split 1 2)",
          "(1 2 nil)");
  expect ("(split 1 2 3 4)", "(1 2 (3 4))");
  expect ("((lambda (a b) (+ a b)) 20 22)", "42");
  // Ordinary lexical shadowing is allowed; only repeated formal names fail.
  expect ("(define args 99) ((lambda args args) 1 2)", "(1 2)");
  expect ("args", "99");
}
END_TEST

START_TEST (test_variadic_arity_errors)
{
  const char *calls[] = { "((lambda (a . extras) extras))",
                          "((lambda (a b . extras) extras) 1)",
                          "((lambda (a b) a) 1)", "((lambda (a b) a) 1 2 3)" };
  for (size_t i = 0; i < sizeof (calls) / sizeof (*calls); ++i)
    {
      char source[256];
      snprintf (source, sizeof (source),
                "(define touched 0) %s (set! touched 1)", calls[i]);
      expect (source, "nil");
      expect ("touched", "0");
    }
  expect ("((lambda args (length args)) 1 2 3)", "3");
  ck_assert_int_eq (lm->stk.sp, 0);
  ck_assert_int_eq (lm->ctl.sp, 0);
  ck_assert_ptr_eq (lm->env.current, lm->env.global);
}
END_TEST

START_TEST (test_invalid_parameter_specifications)
{
  const char *invalid[]
      = { "(lambda (x x) x)",        "(lambda (x . x) x)",
          "(lambda (x y . y) x)",    "(define (f x x) x)",
          "(define (f x . x) x)",    "(lambda 42 1)",
          "(lambda (x . 42) x)",     "(lambda (. args) args)",
          "(lambda (x . args y) x)", "(lambda (x . (y)) x)",
          "(define (f . 42) 1)",     "(define (f . (x)) x)",
          "(lambda NIL 1)",          "(lambda (first) first)",
          "(lambda (x . rest) x)",   "(lambda (quote) 1)" };
  for (size_t i = 0; i < sizeof (invalid) / sizeof (*invalid); ++i)
    {
      Cell *forms = T;
      ck_assert_msg (!parser_buf (invalid[i], &forms, lm),
                     "Should reject parameters in %s", invalid[i]);
      ck_assert_ptr_eq (forms, NIL);
    }
  // Rejection is at definition time, not deferred until a bad function runs.
  expect ("(+ 20 22)", "42");
}
END_TEST

START_TEST (test_variadic_apply_funcall_and_evaluation_order)
{
  expect ("(define sequence 0)"
          "(define (bump x) (set! sequence (+ (* sequence 10) x)) x)"
          "(define (collect . values) values)"
          "(collect (bump 1) (bump 2) (bump 3))",
          "(1 2 3)");
  expect ("sequence", "123");
  expect ("(apply collect (bump 4) (list (bump 5) (bump 6)))", "(4 5 6)");
  expect ("sequence", "123456");
  expect ("(funcall collect (bump 7) (bump 8))", "(7 8)");
  expect ("sequence", "12345678");
  expect ("(apply collect NIL)", "nil");
  expect ("(funcall collect)", "nil");
  expect ("(define (tag head . values) (cons head values))"
          "(apply tag 'a '(1 2))",
          "(a 1 2)");
}
END_TEST

START_TEST (test_rest_bindings_survive_closures_and_gc)
{
  evaluate ("(define (capture head . extras) (lambda () (cons head extras)))"
            "(define saved (capture 'a 1 2 3))"
            "(define empty (capture 'b))");
  lm_gc (lm);
  expect ("(let ((head 99) (extras 100)) (saved))", "(a 1 2 3)");
  expect ("(empty)", "(b)");
  evaluate ("(define (build n acc)"
            " (if (= n 0) acc (build (- n 1) (cons n acc))))"
            "(define xs (build 5000 NIL))"
            "(define copied (apply (lambda args (lambda () args)) xs))");
  lm_gc (lm);
  expect ("(length (copied))", "5000");
  expect ("(nth 4999 (copied))", "5000");
}
END_TEST

START_TEST (test_variadic_tail_recursion)
{
  evaluate ("(define (bounce n . values)"
            " (if (= n 0) values (apply bounce (- n 1) values)))");
  expect ("(bounce 10 'a 'b)", "(a b)");
  size_t shallow_depth = lm->ctl.high_water;
  expect ("(bounce 20000 'a 'b)", "(a b)");
  ck_assert_int_eq (lm->ctl.high_water, shallow_depth);
  ck_assert_uint_lt (lm->ctl.high_water, 64);
}
END_TEST

Suite *
variadic_suite (void)
{
  Suite *suite = suite_create ("Variadic Parameters");
  TCase *tc    = tcase_create ("Bindings");
  tcase_add_checked_fixture (tc, setup, teardown);
  tcase_set_timeout (tc, 30);
  tcase_add_test (tc, test_rest_only_parameters);
  tcase_add_test (tc, test_required_and_rest_parameters);
  tcase_add_test (tc, test_variadic_arity_errors);
  tcase_add_test (tc, test_invalid_parameter_specifications);
  tcase_add_test (tc, test_variadic_apply_funcall_and_evaluation_order);
  tcase_add_test (tc, test_rest_bindings_survive_closures_and_gc);
  tcase_add_test (tc, test_variadic_tail_recursion);
  suite_add_tcase (suite, tc);
  return suite;
}
