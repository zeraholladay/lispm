#include <check.h>
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

START_TEST (test_short_circuit_values_evaluated_once)
{
  expect ("(list (and) (or) (and 0) (or 0))", "(t nil 0 0)");
  evaluate ("(define calls 0) (define object (list 1 2))"
            "(define (produce) (set! calls (+ calls 1)) object)");
  expect ("(eq (or (produce) (error 'unreachable)) object)", "t");
  expect ("calls", "1");
  expect ("(eq (and T (produce)) object)", "t");
  expect ("calls", "2");
  expect ("(and (produce) NIL (error 'unreachable))", "nil");
  expect ("calls", "3");
  expect ("(eq (or NIL (produce) (error 'unreachable)) object)", "t");
  expect ("calls", "4");
  expect ("(or 0 (error 'unreachable))", "0");
}
END_TEST

START_TEST (test_lowering_preserves_binding_scope)
{
  evaluate ("(define tmp 17) (define value 23) (define _or_value 31)");
  expect ("(or NIL (+ tmp value _or_value))", "71");
  // A temporary lambda/let around or would incorrectly hide these defines.
  expect ("(or (define missed NIL) (define created 42))", "42");
  expect ("(list missed created tmp value _or_value)", "(nil 42 17 23 31)");
  expect ("(define (scope) (or NIL (define local 42)) local) (scope)", "42");
  expect ("(and (define present T) (define finished 9))", "9");
  expect ("finished", "9");
  expect ("(let ((value 1)) (or NIL (set! value 2)) value)", "2");
  expect ("value", "23");
}
END_TEST

START_TEST (test_let_parallel_initializers_and_order)
{
  expect ("(define x 100) (let ((x (+ x 1)) (y x)) (list x y))", "(101 100)");
  expect ("x", "100");
  expect ("(define sequence 0)"
          "(define (bump x) (set! sequence (+ (* sequence 10) x)) x)"
          "(let ((a (bump 1)) (b (bump 2))) (list a b sequence))",
          "(1 2 12)");
  expect ("(let () (define local 7) local)", "7");
  // Preserve this dialect's existing duplicate-let-binding behavior.
  expect ("(let ((x 1) (x 2)) x)", "2");
  expect ("(define touched 0)"
          "(let ((x (set! touched 1)) (broken)) x)",
          "nil");
  expect ("touched", "0");
}
END_TEST

START_TEST (test_quote_and_eval_keep_source_data_intact)
{
  evaluate ("(define code '(or NIL 42))"
            "(define local-code '(let ((x 40)) (+ x 2)))");
  expect ("(eval code)", "42");
  expect ("(equal? code '(or NIL 42))", "t");
  expect ("(eval code)", "42");
  expect ("(eval local-code)", "42");
  expect ("(equal? local-code '(let ((x 40)) (+ x 2)))", "t");
  expect ("(eval '(and T 42))", "42");
  // Derived forms reached through another binding use the same lowerer.
  expect ("(define choose or) (eval (cons choose (list NIL 42)))", "42");
  lm_gc (lm);
  expect ("(eval local-code)", "42");
}
END_TEST

START_TEST (test_lowering_does_not_depend_on_prelude_bindings)
{
  evaluate ("(set! not (lambda args (error 'unexpected-helper)))"
            "(set! list? (lambda args (error 'unexpected-helper)))");
  expect ("(and T 42)", "42");
  expect ("(or NIL 42)", "42");
  expect ("(let ((x 42)) x)", "42");
}
END_TEST

START_TEST (test_apply_preserves_fixed_and_final_lists)
{
  evaluate ("(define values (list 3 4))"
            "(define saved-tail (cdr values))"
            "(define joined (apply list 1 2 values))");
  expect ("joined", "(1 2 3 4)");
  expect ("(list (equal? values '(3 4)) (eq (cdr values) saved-tail)"
          "       (eq (cddr joined) values) (eq (apply list values) values))",
          "(t t t t)");
}
END_TEST

START_TEST (test_long_boolean_forms_use_bounded_control)
{
  const char *names[] = { "and", "or" };
  for (size_t form = 0; form < 2; ++form)
    {
      Cell *operands = LIST1 (INTEGER (42, lm), lm);
      for (size_t i = 0; i < 5000; ++i)
        operands = CONS (form == 0 ? T : NIL, operands, lm);
      Cell *call   = CONS (SYMBOL (names[form], strlen (names[form]), lm),
                           operands, lm);
      Cell *result = lm_progn (lm, LIST1 (call, lm));
      ck_assert (IS_INST (result, INTEGER));
      ck_assert_int_eq (result->integer, 42);
      ck_assert_uint_lt (lm->ctl.high_water, 16);
    }
}
END_TEST

Suite *
syntax_suite (void)
{
  Suite *suite = suite_create ("Syntax Lowering");
  TCase *tc    = tcase_create ("Core forms");
  tcase_add_checked_fixture (tc, setup, teardown);
  tcase_set_timeout (tc, 30);
  tcase_add_test (tc, test_short_circuit_values_evaluated_once);
  tcase_add_test (tc, test_lowering_preserves_binding_scope);
  tcase_add_test (tc, test_let_parallel_initializers_and_order);
  tcase_add_test (tc, test_quote_and_eval_keep_source_data_intact);
  tcase_add_test (tc, test_lowering_does_not_depend_on_prelude_bindings);
  tcase_add_test (tc, test_apply_preserves_fixed_and_final_lists);
  tcase_add_test (tc, test_long_boolean_forms_use_bounded_control);
  suite_add_tcase (suite, tc);
  return suite;
}
