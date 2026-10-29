#include <check.h>
#include <stdio.h>

#include "lm.h"
#include "lm_gc.h"
#include "parser.h"
#include "prims.h"

static LM *lm;

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
evaluate (const char *source)
{
  Cell *forms = NULL;
  ck_assert (parser_buf (source, &forms, lm));
  return lm_progn (lm, forms);
}

static void
assert_returned (void)
{
  // Reusing returns must not leave values/states behind or strand the VM
  // in a callee's lexical frame after the top-level expression finishes.
  ck_assert_int_eq (lm->ctl.sp, 0);
  ck_assert_int_eq (lm->stk.sp, 0);
  ck_assert_ptr_eq (lm->env.current, lm->env.global);
}

START_TEST (test_deep_tail_recursion)
{
  evaluate ("(define (countdown n acc)"
            "  (if (eq n 0) acc (countdown (- n 1) (+ acc 1))))");
  Cell *result = evaluate ("(countdown 10 0)");
  ck_assert_int_eq (result->integer, 10);
  size_t shallow_depth = lm->ctl.high_water;

  // Merely completing a large call could hide a raised stack limit. Equal
  // peaks for 10 and 30000 iterations establish bounded continuation usage.
  result = evaluate ("(countdown 30000 0)");
  ck_assert (IS_INST (result, INTEGER));
  ck_assert_int_eq (result->integer, 30000);
  ck_assert_int_eq (lm->ctl.high_water, shallow_depth);
  ck_assert_uint_lt (lm->ctl.high_water, 64);
  assert_returned ();
}
END_TEST

START_TEST (test_mutual_tail_recursion)
{
  // Tail-call elimination must work across functions, not just recognize a
  // call to the currently executing function's name.
  evaluate ("(define (even n) (if (eq n 0) T (odd (- n 1))))"
            "(define (odd n) (if (eq n 0) NIL (even (- n 1))))");
  ck_assert_ptr_eq (evaluate ("(even 20000)"), T);
  ck_assert_uint_lt (lm->ctl.high_water, 64);
  ck_assert_ptr_eq (evaluate ("(even 20001)"), NIL);
  ck_assert_uint_lt (lm->ctl.high_water, 64);
  assert_returned ();
}
END_TEST

START_TEST (test_tail_positions_and_indirect_calls)
{
  const char *calls[] = {
    "(progn 7 (step (- n 1)))",    "(let ((next (- n 1))) 7 (step next))",
    "(and T (step (- n 1)))",      "(or NIL (step (- n 1)))",
    "(apply step (list (- n 1)))", "(funcall step (- n 1))",
    "(eval (list 'step (- n 1)))"
  };
  for (size_t i = 0; i < sizeof (calls) / sizeof (*calls); ++i)
    {
      char source[512];
      // Earlier body expressions must complete; only the final one is tail.
      snprintf (source, sizeof (source),
                "(define (step n) n n (if (> n 0) %s 42))", calls[i]);
      evaluate (source);
      Cell *result = evaluate ("(step 10000)");
      ck_assert_msg (IS_INST (result, INTEGER), "tail path failed: %s",
                     calls[i]);
      ck_assert_int_eq (result->integer, 42);
      ck_assert_msg (lm->ctl.high_water < 64, "stack grew for %s: %zu",
                     calls[i], lm->ctl.high_water);
      assert_returned ();
    }

  // The callee is also allowed to arrive through a parameter. This checks
  // higher-order calls alternating between a named and an anonymous function.
  Cell *result
      = evaluate ("(define (dispatch f n) (f f n))"
                  "(dispatch (lambda (self n)"
                  "  (if (eq n 0) 42 (dispatch self (- n 1)))) 10000)");
  ck_assert (IS_INST (result, INTEGER));
  ck_assert_int_eq (result->integer, 42);
  ck_assert_uint_lt (lm->ctl.high_water, 64);
  assert_returned ();
}
END_TEST

START_TEST (test_non_tail_work_is_preserved)
{
  // The addition after sum returns must remain pending at every level.
  evaluate ("(define (sum n) (if (eq n 0) 0 (+ n (sum (- n 1)))))");
  ck_assert_int_eq (evaluate ("(sum 5)")->integer, 15);
  size_t shallow_depth = lm->ctl.high_water;
  ck_assert_int_eq (evaluate ("(sum 40)")->integer, 820);
  ck_assert_uint_gt (lm->ctl.high_water, shallow_depth);

  // A recursive call in an earlier logical operand or an if predicate is
  // not tail: the caller must still inspect its result and finish its work.
  ck_assert_int_eq (evaluate ("(define (and-work n) (if (eq n 0) T (and "
                              "(and-work (- n 1)) (+ n 1))))"
                              "(and-work 40)")
                        ->integer,
                    41);
  ck_assert_int_eq (
      evaluate (
          "(define (or-work n) (if (eq n 0) NIL (or (or-work (- n 1)) n)))"
          "(or-work 40)")
          ->integer,
      1);
  ck_assert_int_eq (
      evaluate ("(define (predicate-work n)"
                "  (if (eq n 0) T (if (predicate-work (- n 1)) n NIL)))"
                "(predicate-work 40)")
          ->integer,
      40);
  ck_assert_int_eq (
      evaluate ("(define (choose n) (if (eq n 0) + (choose (- n 1))))"
                "((choose 10000) 20 22)")
          ->integer,
      42);
  assert_returned ();
}
END_TEST

START_TEST (test_tail_calls_preserve_escaped_frames)
{
  // Save one closure per iteration. If tail calls overwrote/recycled the
  // caller's frame, these closures would lose their distinct captured n's.
  evaluate ("(define saved NIL)"
            "(define (capture n)"
            "  (set! saved (cons (lambda () n) saved))"
            "  (if (eq n 0) 0 (capture (- n 1))))"
            "(capture 100)");
  lm_gc (lm);
  // They must retain those bindings even after an explicit collection.
  Cell *values = evaluate ("(map (lambda (f) (f)) saved)");
  for (Integer expected = 0; expected <= 100; ++expected)
    {
      ck_assert (CONSP (values));
      ck_assert_int_eq (CAR (values)->integer, expected);
      values = CDR (values);
    }
  ck_assert_ptr_eq (values, NIL);
  assert_returned ();
}
END_TEST

START_TEST (test_caller_environment_is_restored)
{
  // The deep tail-call chain still owes a return to its non-tail caller.
  // That caller must resume in its own frame, including inside let bindings.
  evaluate ("(define (bounce n) (if (eq n 0) 42 (bounce (- n 1))))");
  ck_assert_int_eq (
      evaluate ("(define (caller x) (bounce 10000) x) (caller 777)")->integer,
      777);
  ck_assert_int_eq (
      evaluate ("(let ((x 7)) (let ((y (bounce 10000))) x))")->integer, 7);
  ck_assert_int_eq (evaluate ("(+ (let ((x 9)) (bounce 10000)) 1)")->integer,
                    43);
  assert_returned ();
}
END_TEST

START_TEST (test_tail_call_error_recovery)
{
  // A failure at the end of a long optimized chain must unwind to a clean
  // top-level VM, ready to evaluate the next expression normally.
  evaluate ("(define (fail n) (if (eq n 0) (fail) (fail (- n 1))))");
  ck_assert_ptr_eq (evaluate ("(fail 10000)"), NIL);
  assert_returned ();
  ck_assert_int_eq (evaluate ("(+ 20 22)")->integer, 42);
  // A final primitive has no sequencing state left to notice its error.
  ck_assert_ptr_eq (evaluate ("(/ 1 0)"), NIL);
  assert_returned ();
  ck_assert_int_eq (evaluate ("(+ 20 22)")->integer, 42);
}
END_TEST

Suite *
tail_call_suite (void)
{
  Suite *suite = suite_create ("Tail Calls");
  TCase *tc    = tcase_create ("Continuations");
  tcase_add_checked_fixture (tc, setup, teardown);
  tcase_set_timeout (tc, 30);
  tcase_add_test (tc, test_deep_tail_recursion);
  tcase_add_test (tc, test_mutual_tail_recursion);
  tcase_add_test (tc, test_tail_positions_and_indirect_calls);
  tcase_add_test (tc, test_non_tail_work_is_preserved);
  tcase_add_test (tc, test_tail_calls_preserve_escaped_frames);
  tcase_add_test (tc, test_caller_environment_is_restored);
  tcase_add_test (tc, test_tail_call_error_recovery);
  suite_add_tcase (suite, tc);
  return suite;
}
