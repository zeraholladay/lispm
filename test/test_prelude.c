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

START_TEST (test_loaded_and_redefinable)
{
  ck_assert (IS_INST (evaluate ("identity"), LAMBDA));
  expect ("(identity 42)", "42");
  lm_gc (lm);
  expect ("(fold-left + 0 '(1 2 3))", "6");
  expect ("(define (identity value) 99) (identity 42)", "99");
  expect ("(set! identity (lambda (value) value)) (identity 42)", "42");
  ck_assert_ptr_eq (lm->env.current, lm->env.global);
}
END_TEST

START_TEST (test_migrated_procedures_are_normal_bindings)
{
  const char *names[] = { "not",     "length", "reverse", "nth", "last",
                          "butlast", "list",   "append",  "map" };
  lm_gc (lm);
  for (size_t i = 0; i < sizeof (names) / sizeof (*names); ++i)
    {
      ck_assert_msg (IS_INST (evaluate (names[i]), LAMBDA),
                     "%s should be a Scheme procedure", names[i]);
      char source[256];
      snprintf (source, sizeof (source),
                "(define saved-procedure %s)"
                "(set! %s (lambda () 42)) (%s)",
                names[i], names[i], names[i]);
      expect (source, "42");
      snprintf (source, sizeof (source), "(set! %s saved-procedure)",
                names[i]);
      ck_assert (IS_INST (evaluate (source), LAMBDA));
    }
  // Internal VM application must not depend on redefinable library helpers.
  expect ("(set! last (lambda (xs) 99)) (apply + '(1 2 3))", "6");
  expect ("(define (length xs) 99) (length NIL)", "99");
}
END_TEST

START_TEST (test_migrated_sequence_contracts)
{
  expect ("(list (not NIL) (not T) (not 0) (not 0.0) (not '(a)))",
          "(t nil nil nil nil)");
  expect ("(list (length NIL) (length '(a)) (length '(() a (b c))))",
          "(0 1 3)");
  expect ("(list (reverse NIL) (reverse '(a)) (reverse '(a b c)))",
          "(nil (a) (c b a))");
  expect ("(list (nth 0 '(a b c)) (nth 2 '(a b c)) (nth 3 '(a b c))"
          " (nth -1 '(a)) (nth 0 NIL)"
          " (nth -9223372036854775808 '(a))"
          " (nth 9223372036854775807 '(a)))",
          "(a c nil nil nil nil nil)");
  expect ("(list (last NIL) (last '(a)) (last '(a b c)))", "((nil) (a) (c))");
  expect ("(list (butlast NIL) (butlast '(a)) (butlast '(a b c)))",
          "(nil nil (a b))");
  expect ("(map length '(() (a) (a b)))", "(0 1 2)");
  expect ("(apply nth '(1 (a b c)))", "b");
  expect ("(funcall reverse '(a b c))", "(c b a)");
}
END_TEST

START_TEST (test_migrated_sequence_identity_and_nonmutation)
{
  evaluate ("(define element (list 1))"
            "(define xs (list element 2 3))"
            "(define original-tail (cdr xs))");
  expect ("(reverse xs)", "(3 2 (1))");
  expect ("(last xs)", "(3)");
  expect ("(butlast xs)", "((1) 2)");
  expect ("(list (equal? xs '((1) 2 3)) (eq (cdr xs) original-tail)"
          " (eq (nth 0 xs) element) (eq (car (butlast xs)) element))",
          "(t t t t)");
  expect ("(let ((single (list element)))"
          " (list (eq (reverse single) single) (eq (last single) single)"
          "       (eq (car (last single)) element)))",
          "(nil nil t)");
}
END_TEST

START_TEST (test_migrated_sequence_validation)
{
  const char *invalid[] = { "(not)",
                            "(not 1 2)",
                            "(length)",
                            "(length NIL NIL)",
                            "(reverse)",
                            "(reverse NIL NIL)",
                            "(nth 0)",
                            "(nth 0 NIL NIL)",
                            "(last)",
                            "(last NIL NIL)",
                            "(butlast)",
                            "(butlast NIL NIL)",
                            "(length '(a . b))",
                            "(reverse 'a)",
                            "(nth 0 '(a . b))",
                            "(nth -1 '(a . b))",
                            "(nth 1.0 '(a b))",
                            "(nth 'a NIL)",
                            "(last '(a . b))",
                            "(butlast 42)" };
  for (size_t i = 0; i < sizeof (invalid) / sizeof (*invalid); ++i)
    {
      char source[256];
      snprintf (source, sizeof (source),
                "(define touched 0) %s (set! touched 1)", invalid[i]);
      expect (source, "nil");
      // These must be actual errors, not merely successful NIL results.
      expect ("touched", "0");
    }
  Cell *cycle = evaluate ("(define cycle (list 1))");
  RPLACD (cycle, cycle);
  const char *cyclic[]
      = { "(length cycle)", "(reverse cycle)", "(nth 0 cycle)", "(last cycle)",
          "(butlast cycle)" };
  for (size_t i = 0; i < sizeof (cyclic) / sizeof (*cyclic); ++i)
    expect (cyclic[i], "nil");
  expect ("(+ 20 22)", "42");
}
END_TEST

START_TEST (test_native_foundations)
{
  expect ("(list (pair? '(a . b)) (pair? NIL) (number? 1.5)"
          " (number? 'a) (integer? 1) (integer? 1.0)"
          " (symbol? 'a) (symbol? 1) (procedure? +)"
          " (procedure? identity) (procedure? 1))",
          "(t nil t nil t nil t nil t t nil)");
  expect ("(list (= 1 1.0 1) (= 1 1.5) (= 7) (= -0.0 0))", "(t nil t t)");
  // Mixed equality must not round an exact integer before comparing it.
  expect ("(= 9007199254740993 9007199254740992.0)", "nil");
  expect ("(= 9223372036854775807 9223372036854775808.0)", "nil");
  expect ("(= -9223372036854775808 -9223372036854775808.0)", "t");
  expect ("(=)", "nil");
  expect ("(= 1 'a)", "nil");
  expect ("(pair?)", "nil");
  expect ("(error 'requested-failure)", "nil");
  expect ("(+ 20 22)", "42");
}
END_TEST

START_TEST (test_first_and_rest_are_redefinable_aliases)
{
  expect ("(list (eq first car) (eq rest cdr))", "(t t)");
  expect ("(first '(1 . 2))", "1");
  expect ("(rest '(1 . 2))", "2");
  expect ("(set! first (lambda (xs) 99)) (first NIL)", "99");
  expect ("(define (rest xs) 88) (rest NIL)", "88");
  expect ("((lambda (first . rest) (cons first rest)) 1 2 3)", "(1 2 3)");
}
END_TEST

START_TEST (test_predicates_and_numeric_helpers)
{
  expect ("(list (null? NIL) (null? '(NIL)) (zero? 0) (zero? 0.0)"
          " (zero? 3) (positive? 1.5) (negative? -2)"
          " (<= 2 2.0) (<= 3 2) (>= 2 2) (>= 1 2))",
          "(t nil t t nil t t t nil t nil)");
  expect ("(list (abs -7) (abs 7) (abs 0))", "(7 7 0)");
  expect ("(= (abs -1.5) 1.5)", "t");
  expect ("(zero? 'a)", "nil");
  expect ("(abs -9223372036854775808)", "nil");
}
END_TEST

START_TEST (test_accessors)
{
  expect ("(list (caar '((1 2) 3)) (cadr '(1 2 3))"
          " (cdar '((1 2) 3)) (cddr '(1 2 3)) (caddr '(1 2 3)))",
          "(1 2 (2) (3) 3)");
  expect ("(cdar '((a . b)))", "b");
  expect ("(cadr '(a . b))", "nil");
}
END_TEST

START_TEST (test_list_shape_and_structural_equality)
{
  expect ("(list (list? NIL) (list? '(1 2)) (list? '(1 . 2))"
          " (list? 'a) (list? 42) (list? '((a . b))))",
          "(t t nil nil nil t)");
  expect ("(list (equal? '(1 (2 . 3)) '(1.0 (2.0 . 3)))"
          " (equal? '(1 2) '(1 3)) (equal? '(a . b) '(a b))"
          " (equal? identity identity) (equal? NIL NIL))",
          "(t nil nil t t)");
  Cell *cycle = evaluate ("(define cycle (list 1 2 3))");
  RPLACD (CDDR (cycle), cycle);
  expect ("(list? cycle)", "nil");
  expect ("(filter identity cycle)", "nil");
}
END_TEST

START_TEST (test_folds_and_callback_order)
{
  expect ("(fold-left - 0 '(1 2 3))", "-6");
  expect ("(fold-right - 0 '(1 2 3))", "2");
  expect ("(fold-right cons NIL '(1 2 3))", "(1 2 3)");
  expect ("(list (fold-left error 7 NIL) (fold-right error 8 NIL))", "(7 8)");
  expect ("(define seen NIL)"
          "(fold-left (lambda (acc x) (set! seen (cons x seen)) (+ acc x))"
          " 0 '(1 2 3)) seen",
          "(3 2 1)");
  expect ("(set! seen NIL)"
          "(fold-right (lambda (x acc) (set! seen (cons x seen)) (+ x acc))"
          " 0 '(1 2 3)) seen",
          "(1 2 3)");
}
END_TEST

START_TEST (test_filter_and_find)
{
  expect ("(filter positive? '(-1 0 2 3 -4 5))", "(2 3 5)");
  expect ("(filter positive? NIL)", "nil");
  expect ("(let ((xs '(1 2 3))) (let ((ys (filter positive? xs)))"
          " (list (equal? xs ys) (eq xs ys))))",
          "(t nil)");
  expect ("(find negative? '(1 -2 -3))", "-2");
  expect ("(find negative? '(1 2))", "nil");
  expect ("(find error NIL)", "nil");
  expect ("(list (find null? '(1 () 3)) (any? null? '(1 () 3)))", "(nil t)");
}
END_TEST

START_TEST (test_short_circuiting_and_for_each)
{
  expect ("(list (any? error NIL) (every? error NIL) (for-each error NIL))",
          "(nil t t)");
  expect ("(define visits 0)"
          "(any? (lambda (x) (set! visits (+ visits 1)) (> x 1)) '(1 2 3))"
          " visits",
          "2");
  expect ("(set! visits 0)"
          "(every? (lambda (x) (set! visits (+ visits 1)) (positive? x))"
          " '(1 -1 3)) visits",
          "2");
  // Zero is truthy in this dialect; predicates normalize it to literal T.
  expect ("(list (any? (constantly 0) '(1))"
          " (every? (constantly 0) '(1 2)))",
          "(t t)");
  expect ("(define seen NIL)"
          "(for-each (lambda (x) (set! seen (cons x seen))) '(1 2 3))"
          " seen",
          "(3 2 1)");
  expect ("(for-each (lambda (x) NIL) '(1 2 3))", "t");
}
END_TEST

START_TEST (test_membership_and_associations)
{
  expect ("(memq 'b '(a b c))", "(b c)");
  expect ("(memq NIL '(a () c))", "(nil c)");
  expect ("(memq 'z '(a b c))", "nil");
  expect ("(let ((xs '(a b c))) (eq (memq 'b xs) (cdr xs)))", "t");
  expect ("(let ((element (list 1)))"
          " (list (null? (memq (list 1) (list element)))"
          "       (pair? (memq element (list element)))))",
          "(t t)");
  expect ("(assq 'name '((name . alice) (age . 42) (name . bob)))",
          "(name . alice)");
  expect ("(assq 'missing '((name . alice)))", "nil");
  expect ("(let ((entries '((name . alice))))"
          " (eq (assq 'name entries) (car entries)))",
          "t");
  expect ("(assq 'name '((name . alice) 42))", "(name . alice)");
}
END_TEST

START_TEST (test_higher_order_closures)
{
  expect ("(map (constantly 7) '(a b c))", "(7 7 7)");
  expect ("(let ((value (list 1))) (eq ((constantly value) 999) value))", "t");
  expect ("((compose abs (lambda (x) (- x 10))) 3)", "7");
  ck_assert (IS_INST (
      evaluate ("(define combined (compose abs (lambda (x) (- x 10))))"),
      LAMBDA));
  lm_gc (lm);
  expect ("(combined 3)", "7");
}
END_TEST

START_TEST (test_invalid_arguments_and_error_abort)
{
  const char *invalid[] = { "(fold-left + 0 '(1 . 2))",
                            "(fold-right + 0 'a)",
                            "(filter 42 NIL)",
                            "(filter positive? '(1 . 2))",
                            "(find + 'a)",
                            "(any? 42 '(1))",
                            "(every? positive? '(1 . 2))",
                            "(for-each + 3)",
                            "(memq 'a '(a . b))",
                            "(assq 'a '(42))",
                            "(compose 1 +)" };
  for (size_t i = 0; i < sizeof (invalid) / sizeof (*invalid); ++i)
    expect (invalid[i], "nil");
  expect ("(define touched 0) (error 'stop-here) (set! touched 1)", "nil");
  expect ("touched", "0");
  expect ("(+ 20 22)", "42");
}
END_TEST

START_TEST (test_long_list_tail_traversal)
{
  evaluate ("(define (build n acc)"
            " (if (= n 0) acc (build (- n 1) (cons n acc))))"
            "(define xs (build 5000 NIL))");
  expect ("(fold-left + 0 xs)", "12502500");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
  expect ("(length (filter positive? xs))", "5000");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
  expect ("(find (lambda (x) (= x 5000)) xs)", "5000");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
  expect ("(length xs)", "5000");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
  expect ("(car (reverse xs))", "5000");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
  expect ("(nth 4999 xs)", "5000");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
  expect ("(last xs)", "(5000)");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
  expect ("(length (butlast xs))", "4999");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
}
END_TEST

START_TEST (test_variadic_list_values)
{
  expect ("(list)", "nil");
  expect ("(list 1 'a NIL '(b c))", "(1 a nil (b c))");
  expect ("(let ((element '(a b))) (eq (car (list element)) element))", "t");
  expect ("(apply list '(1 2 3))", "(1 2 3)");
  expect ("(funcall list 1 2 3)", "(1 2 3)");
  expect ("(let ((xs '(1 2 3))) (eq (apply list xs) xs))", "t");
}
END_TEST

START_TEST (test_variadic_append_sharing)
{
  expect ("(append)", "nil");
  expect ("(append NIL)", "nil");
  expect ("(append '(1 2) NIL '(3) '(4 5))", "(1 2 3 4 5)");
  expect ("(let ((xs '(1 2))) (eq (append xs) xs))", "t");
  expect ("(let ((xs '(1 2))) (eq (append NIL NIL xs) xs))", "t");
  expect ("(let ((prefix '(1 2)) (tail '(3 4)))"
          " (let ((joined (append prefix tail)))"
          "   (list (equal? joined '(1 2 3 4)) (equal? prefix '(1 2))"
          "         (eq (cddr joined) tail) (eq joined prefix))))",
          "(t t t nil)");
  expect ("(let ((xs '(1 2))) (list (equal? (append xs NIL) xs)"
          "                               (eq (append xs NIL) xs)))",
          "(t nil)");
  expect ("(apply append '((1 2) (3 4)))", "(1 2 3 4)");
}
END_TEST

START_TEST (test_variadic_map_and_indirect_calls)
{
  expect ("(map + '(1 2 3) '(10 20))", "(11 22)");
  expect ("(map list '(a b) '(1 2) '(x y z))", "((a 1 x) (b 2 y))");
  expect ("(map list '(a b) '(1))", "((a 1))");
  expect ("(map list NIL '(1))", "nil");
  expect ("(map list '(1) NIL)", "nil");
  expect ("(apply map (list + '(1 2 3) '(4 5 6)))", "(5 7 9)");
  expect ("(funcall map + '(1 2) '(10 20))", "(11 22)");
  expect ("(define seen NIL)"
          "(map (lambda (a b) (set! seen (cons a seen)) (- a b))"
          " '(10 20) '(1 2))",
          "(9 18)");
  expect ("seen", "(20 10)");
  expect ("(let ((element '(a b)))"
          " (let ((xs (list element)))"
          "   (let ((ys (map identity xs)))"
          "     (list (eq (car ys) element) (eq xs ys) (equal? xs ys)))))",
          "(t nil t)");
}
END_TEST

START_TEST (test_variadic_list_argument_errors)
{
  const char *invalid[] = { "(append '(1) 2)",
                            "(append NIL '(1 . 2))",
                            "(append 'a)",
                            "(map)",
                            "(map +)",
                            "(map 42 NIL)",
                            "(map + NIL '(a . b))",
                            "(map + 'a)",
                            "(map (lambda (x y) x) '(1))" };
  for (size_t i = 0; i < sizeof (invalid) / sizeof (*invalid); ++i)
    {
      char source[256];
      snprintf (source, sizeof (source),
                "(define touched 0) %s (set! touched 1)", invalid[i]);
      expect (source, "nil");
      expect ("touched", "0");
    }
  expect ("(define visits 0)"
          "(map (lambda (a b) (set! visits (+ visits 1)) a) NIL '(a . b))",
          "nil");
  expect ("visits", "0");
  Cell *cycle = evaluate ("(define cycle (list 1))");
  RPLACD (cycle, cycle);
  expect ("(append NIL cycle)", "nil");
  expect ("(map list NIL cycle)", "nil");
}
END_TEST

START_TEST (test_variadic_traversal_control_bounds)
{
  evaluate ("(define (columns n acc)"
            " (if (= n 0) acc (columns (- n 1) (cons '(1) acc))))"
            "(define inputs (columns 5000 NIL))");
  // The number of input lists must not become recursive C/VM stack depth.
  expect ("(car (apply map + inputs))", "5000");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
  expect ("(length (apply append inputs))", "5000");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
  evaluate ("(define (build n acc)"
            " (if (= n 0) acc (build (- n 1) (cons n acc))))"
            "(define xs (build 1000 NIL))"
            "(define summed (map + xs xs))");
  ck_assert_uint_lt (lm->ctl.high_water, 128);
  expect ("(list (length summed) (car summed) (last summed))",
          "(1000 2 (2000))");
}
END_TEST

Suite *
prelude_suite (void)
{
  Suite *suite = suite_create ("Prelude");
  TCase *tc    = tcase_create ("Library");
  tcase_add_checked_fixture (tc, setup, teardown);
  tcase_set_timeout (tc, 30);
  tcase_add_test (tc, test_loaded_and_redefinable);
  tcase_add_test (tc, test_migrated_procedures_are_normal_bindings);
  tcase_add_test (tc, test_migrated_sequence_contracts);
  tcase_add_test (tc, test_migrated_sequence_identity_and_nonmutation);
  tcase_add_test (tc, test_migrated_sequence_validation);
  tcase_add_test (tc, test_native_foundations);
  tcase_add_test (tc, test_first_and_rest_are_redefinable_aliases);
  tcase_add_test (tc, test_predicates_and_numeric_helpers);
  tcase_add_test (tc, test_accessors);
  tcase_add_test (tc, test_list_shape_and_structural_equality);
  tcase_add_test (tc, test_folds_and_callback_order);
  tcase_add_test (tc, test_filter_and_find);
  tcase_add_test (tc, test_short_circuiting_and_for_each);
  tcase_add_test (tc, test_membership_and_associations);
  tcase_add_test (tc, test_higher_order_closures);
  tcase_add_test (tc, test_invalid_arguments_and_error_abort);
  tcase_add_test (tc, test_long_list_tail_traversal);
  tcase_add_test (tc, test_variadic_list_values);
  tcase_add_test (tc, test_variadic_append_sharing);
  tcase_add_test (tc, test_variadic_map_and_indirect_calls);
  tcase_add_test (tc, test_variadic_list_argument_errors);
  tcase_add_test (tc, test_variadic_traversal_control_bounds);
  suite_add_tcase (suite, tc);
  return suite;
}
