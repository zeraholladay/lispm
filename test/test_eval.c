#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fmt.h"
#include "lm.h"
#include "lm_gc.h"
#include "palloc.h"
#include "parser.h"
#include "prims.h"
#include "repl.h"
#include "types.h"

static Cell *progn = NULL;
static LM   *lm    = NULL;

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

  eval_res = run_eval_progn ("42.0");
  ck_assert (eval_res->_float == 42.0);

  eval_res = run_eval_progn ("-42.0");
  ck_assert (eval_res->_float == -42.0);

  eval_res = run_eval_progn ("t");
  ck_assert_ptr_eq (eval_res, T);

  eval_res = run_eval_progn ("NIL");
  ck_assert (NILP (eval_res));

  eval_res = run_eval_progn ("'foo");
  ck_assert_str_eq (eval_res->symbol.str, "foo");
}
END_TEST

START_TEST (test_quote)
{
  Cell *eval_res = NULL;
  Cell *car      = NULL;
  Cell *cdr      = NULL;

  // NULL progn
  eval_res = run_eval_progn ("()");
  ck_assert (eval_res);

  eval_res = run_eval_progn ("'(foo)");
  ck_assert (!NILP (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert_str_eq (car->symbol.str, "foo");
  ck_assert (NILP (cdr));

  eval_res = run_eval_progn ("'(foo bar)");
  ck_assert (!NILP (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert (!NILP (cdr));
  ck_assert_str_eq (car->symbol.str, "foo");
  ck_assert_str_eq (CAR (cdr)->symbol.str, "bar");

  eval_res = run_eval_progn ("'(a b . c)");
  ck_assert_str_eq (CAR (eval_res)->symbol.str, "a");
  ck_assert_str_eq (CADR (eval_res)->symbol.str, "b");
  ck_assert_str_eq (CDDR (eval_res)->symbol.str, "c");
}
END_TEST

START_TEST (test_cons)
{
  Cell *eval_res = NULL;
  Cell *car      = NULL;
  Cell *cdr      = NULL;

  eval_res = run_eval_progn ("(cons 'foo 'bar)");
  ck_assert (!NILP (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert_str_eq (car->symbol.str, "foo");
  ck_assert_str_eq (cdr->symbol.str, "bar");

  eval_res = run_eval_progn ("(cons 'foo '(bar))");
  ck_assert (!NILP (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert (!NILP (cdr));
  ck_assert_str_eq (CAR (cdr)->symbol.str, "bar");
}
END_TEST

START_TEST (test_define_set_and_lookup)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(define foo 42)");
  ck_assert (eval_res->integer == 42);

  eval_res = run_eval_progn ("foo");
  ck_assert (eval_res->integer == 42);

  eval_res = run_eval_progn ("(define bar 'foo)");
  ck_assert (IS_INST (eval_res, SYMBOL));

  eval_res = run_eval_progn ("(set! bar '(1 2 3))");
  ck_assert (IS_INST (eval_res, CONS));

  eval_res = run_eval_progn ("(set! bar (lambda () ()))");
  ck_assert (IS_INST (eval_res, LAMBDA));
}
END_TEST

START_TEST (test_first)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(first '())");
  ck_assert (NILP (eval_res));

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
  ck_assert (NILP (eval_res));

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
  ck_assert (NILP (eval_res));

  eval_res = run_eval_progn ("(if (< 2 3) 'yes 'no)");
  ck_assert_str_eq (eval_res->symbol.str, "yes");
}
END_TEST

START_TEST (test_list)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(list)");
  ck_assert (NILP (eval_res));

  eval_res = run_eval_progn ("(list 7)");
  ck_assert_int_eq (CAR (eval_res)->integer, 7);
  ck_assert (NILP (CDR (eval_res)));

  eval_res = run_eval_progn ("(list 1 2 3)");
  ck_assert_int_eq (CAR (eval_res)->integer, 1);
  ck_assert_int_eq (CAR (CDR (eval_res))->integer, 2);
  ck_assert_int_eq (CAR (CDR (CDR (eval_res)))->integer, 3);
  ck_assert (NILP (CDR (CDR (CDR (eval_res)))));

  eval_res     = run_eval_progn ("(list 'a 'b)");
  Cell *second = CAR (CDR (eval_res));
  ck_assert_str_eq (CAR (eval_res)->symbol.str, "a");
  ck_assert_str_eq (second->symbol.str, "b");
  ck_assert (NILP (CDR (CDR (eval_res))));
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
  ck_assert (NILP (eval_res));

  // run
  eval_res = run_eval_progn ("((lambda () (cons 'a 'b) 42))");
  ck_assert (IS_INST (eval_res, INTEGER) && eval_res->integer == 42);

  // run body with a=42
  eval_res = run_eval_progn ("((lambda (a) a) 42)");
  ck_assert (IS_INST (eval_res, INTEGER) && eval_res->integer == 42);

  // define foo and run
  eval_res = run_eval_progn ("(define foo (lambda () (cons 'a 'b)))"
                             "(foo)");
  ck_assert (IS_INST (eval_res, CONS));

  // with parameters
  eval_res = run_eval_progn ("(define foo (lambda (a b) (cons a b)))"
                             "(foo 'bar 'biz)");
  ck_assert (IS_INST (eval_res, CONS));
  ck_assert_str_eq (CAR (eval_res)->symbol.str, "bar");

  // test lexical scope
  eval_res = run_eval_progn ("(define foo 'bar)"
                             "((lambda () foo))");
  ck_assert_str_eq (eval_res->symbol.str, "bar");

  eval_res = run_eval_progn ("((let ((x 7)) (lambda () x)))");
  ck_assert_int_eq (eval_res->integer, 7);
}
END_TEST

START_TEST (test_apply)
{
  Cell *eval_res = NULL;
  Cell *car      = NULL;
  Cell *cdr      = NULL;

  eval_res = run_eval_progn ("(apply (lambda () ()) '())");
  ck_assert (NILP (eval_res));

  // (apply #'+ 1 2 3 '(4 5 6))

  eval_res = run_eval_progn ("(apply (lambda (a b) (cons a b)) '(foo bar))");
  ck_assert (!NILP (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert_str_eq (car->symbol.str, "foo");
  ck_assert_str_eq (cdr->symbol.str, "bar");

  eval_res = run_eval_progn ("(apply cons '(42 a))");
  ck_assert (CAR (eval_res)->integer == 42);

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
  Cell *car      = NULL;
  Cell *cdr      = NULL;

  eval_res = run_eval_progn ("(funcall (lambda () ()))");
  ck_assert (NILP (eval_res));

  eval_res = run_eval_progn ("(funcall (lambda (a b) (cons a b)) 'foo 'bar)");
  ck_assert (!NILP (eval_res));
  car = CAR (eval_res);
  cdr = CDR (eval_res);
  ck_assert_str_eq (car->symbol.str, "foo");
  ck_assert_str_eq (cdr->symbol.str, "bar");

  eval_res = run_eval_progn ("(funcall cons 42 'a)");
  ck_assert (CAR (eval_res)->integer == 42);

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

  eval_res = run_eval_progn ("(apply + (butlast '(1 2 3 4)))");
  ck_assert (eval_res->integer == 6);
}
END_TEST

// (map (lambda (x) (+ x 10)) '(1 2 3 4)) => (11 12 13 14)
// (map #'round '(1.3 2.7 3.4 4.5)) => (1 3 3 4)
// (map #'list '(123 symbol "string" 345) '(1 2 3)) => ((123 1) (SYMBOL 2)
// ("string" 3)) (map #'* '(3 4 5) '(4 5 6)) => (12 20 30)
START_TEST (test_map)
{
  Cell *eval_res = NULL;

  eval_res
      = run_eval_progn ("(apply + (map (lambda (x) (+ x 10)) '(1 2 3 4)))");
  ck_assert (eval_res->integer == 50);

  eval_res = run_eval_progn ("(apply + (map * '(3 4 5) '(4 5 6)))");
  ck_assert (eval_res->integer == 62);
}
END_TEST

START_TEST (test_define)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn ("(define (square x) (* x x))"
                             "(square 12)");
  ck_assert_int_eq (eval_res->integer, 144);

  eval_res = run_eval_progn ("(define (range x max)"
                             "    (if (> x max)"
                             "        nil"
                             "        (cons x (range (+ x 1) max))"
                             "    )"
                             ")"
                             "(apply + (range 0 100))");
  ck_assert_int_eq (eval_res->integer, 5050);

  eval_res = run_eval_progn ("(define (fact n)"
                             "  (define (loop k acc)"
                             "    (if (eq 0 k)"
                             "        acc"
                             "        (loop (- k 1) (* acc k))))"
                             "  (loop n 1))"
                             "(fact 20)");
  ck_assert_int_eq (eval_res->integer, 2432902008176640000);

  eval_res = run_eval_progn ("(define (fib x)"
                             "  (if (or (< x 1) (eq x 1))"
                             "      x"
                             "      (let ((x (sub x 1))"
                             "            (y (sub x 2)))"
                             "          (+ (fib x) (fib y))"
                             "      )"
                             "  )"
                             ")"
                             "(fib 5)");
  ck_assert_int_eq (eval_res->integer, 5);

  eval_res = run_eval_progn ("(fib 10)");
  ck_assert_int_eq (eval_res->integer, 55);

  eval_res = run_eval_progn ("(define (sum-to n)"
                             "  (define (loop i acc)"
                             "    (if (> i n)"
                             "        acc"
                             "        (loop (+ i 1) (+ acc i))))"
                             "  (loop 1 0))"
                             "(sum-to 10)");
  ck_assert_int_eq (eval_res->integer, 55);
}
END_TEST

START_TEST (test_let)
{
  Cell *eval_res = NULL;

  eval_res = run_eval_progn (
      "(define a 1)"
      "(define b 2)"
      "(apply + (let ((c (+ a 2)) (d (+ b 2))) (list a b c d)))");
  ck_assert (eval_res->integer == 10);
}
END_TEST

// ChatGPT tests

START_TEST (test_and_or)
{
  Cell *r;

  // and with zero args → T
  r = run_eval_progn ("(and)");
  ck_assert_ptr_eq (r, T);

  // and stops on NIL
  r = run_eval_progn ("(and T NIL (error))");
  ck_assert_ptr_eq (r, NIL);

  // and returns last truthy
  r = run_eval_progn ("(and T 1 2 3)");
  ck_assert_int_eq (r->integer, 3);

  // or with zero args → NIL
  r = run_eval_progn ("(or)");
  ck_assert_ptr_eq (r, NIL);

  // or stops on T
  r = run_eval_progn ("(or NIL T (error))");
  ck_assert_ptr_eq (r, T);

  // or returns first truthy
  r = run_eval_progn ("(or NIL 0 42)");
  ck_assert_int_eq (r->integer, 0);
}
END_TEST

START_TEST (test_progn)
{
  Cell *r;

  r = run_eval_progn ("(progn)");
  ck_assert_ptr_eq (r, NIL);

  r = run_eval_progn ("(progn 1 2 3)");
  ck_assert_int_eq (r->integer, 3);

  // side-effect ordering: define x then use
  r = run_eval_progn ("(progn (define x 10) (set! x (+ x 5)) x)");
  ck_assert_int_eq (r->integer, 15);
}
END_TEST

START_TEST (test_map_edge_cases)
{
  Cell *r;

  // map over empty list
  r = run_eval_progn ("(map (lambda (x) x) '())");
  ck_assert_ptr_eq (r, NIL);

  // map over multiple lists of differing lengths → should truncate
  r = run_eval_progn ("(map (lambda (a b) (+ a b)) '(1 2 3) '(10 20))");
  // expects (11 22)
  ck_assert_int_eq (CADR (r)->integer, 22);
  ck_assert_ptr_eq (CDDR (r), NIL);

  r = run_eval_progn ("(map + 1)");
  ck_assert_ptr_eq (r, NIL);

  r = run_eval_progn ("(map (lambda (x) x) 'a)");
  ck_assert_ptr_eq (r, NIL);

  // apply map via apply
  // r = run_eval_progn ("(apply map '( + (1 2 3) (4 5 6)))");
  // ck_assert_int_eq (CADR (r)->integer, 7);
}

END_TEST

START_TEST (test_error_arity)
{
  // first with no args → error
  Cell *r = run_eval_progn ("(first)");
  ck_assert (IS_INST (r, NIL));

  // length without args → error
  r = run_eval_progn ("(length)");
  ck_assert (IS_INST (r, NIL));

  // cons with one arg → error
  r = run_eval_progn ("(cons 'a)");
  ck_assert (IS_INST (r, NIL));

  r = run_eval_progn ("(define x 1 2)");
  ck_assert_ptr_eq (r, NIL);

  r = run_eval_progn ("(set! x 1 2)");
  ck_assert_ptr_eq (r, NIL);
}
END_TEST

START_TEST (test_nested_let_lexical_scope)
{
  // inner let shouldn't override outer
  const char *prog = "(define x 100) "
                     "(let ((x 1) (x 2)) x) "
                     "x";
  Cell       *r    = run_eval_progn (prog);
  ck_assert_int_eq (r->integer, 100);
}
END_TEST

START_TEST (test_invalid_let_bindings)
{
  Cell *r = run_eval_progn ("(let ((x)) x)");
  ck_assert_ptr_eq (r, NIL);

  r = run_eval_progn ("(let ((x 1 2)) x)");
  ck_assert_ptr_eq (r, NIL);
}
END_TEST

START_TEST (test_parser_errors)
{
  Cell *ast = NULL;

  ck_assert (!parser_buf ("(", &ast, lm));
  ck_assert (!parser_buf (")", &ast, lm));
  ck_assert (!parser_buf ("@", &ast, lm));
}
END_TEST

START_TEST (test_gc_reclaims_unreachable_cells)
{
  Cell *live = run_eval_progn ("(define x 42)");
  lm_gc (lm);
  ck_assert (!pool_gc_is_free (live));

  run_eval_progn ("(set! x 1)");
  lm_gc (lm);
  ck_assert (pool_gc_is_free (live));

  Cell *floating = run_eval_progn ("(define y 42.0)");
  lm_gc (lm);
  ck_assert (!pool_gc_is_free (floating));
  ck_assert_float_eq (floating->_float, 42.0);
}
END_TEST

START_TEST (test_quote_edge)
{
  Cell *r;
  // nested quoting
  r = run_eval_progn ("'('foo)");
  // equivalent to (quote (quote foo))
  // so r should be the list (quote foo)
  ck_assert (!NILP (r));
  ck_assert_str_eq (CAAR (r)->symbol.str, "quote");
}
END_TEST

START_TEST (test_closure_shared_locations)
{
  Cell *r = run_eval_progn (
      "(define (make-counter) (let ((x 0)) (lambda () (set! x (+ x 1)) x)))"
      "(define counter (make-counter)) (counter)");
  ck_assert (IS_INST (r, INTEGER));
  ck_assert_int_eq (r->integer, 1);
  lm_gc (lm);
  r = run_eval_progn ("(counter)");
  ck_assert_int_eq (r->integer, 2);

  r = run_eval_progn ("(define pair (let ((x 0))"
                      "  (list (lambda () (set! x (+ x 1))) (lambda () x))))"
                      "(funcall (first pair)) (funcall (first (rest pair)))");
  ck_assert_int_eq (r->integer, 1);

  r = run_eval_progn (
      "(define x 10) (define f ((lambda (x) (lambda () x)) 20))"
      "(set! x 30) (let ((x 99)) (f))");
  ck_assert_int_eq (r->integer, 20);
  r = run_eval_progn ("x");
  ck_assert_int_eq (r->integer, 30);
}
END_TEST

START_TEST (test_gc_collects_closure_cycles)
{
  Cell *closure = run_eval_progn (
      "(define escaped (let () (define (loop) (loop)) loop))");
  ck_assert (IS_INST (closure, LAMBDA));
  Cell *frame = closure->lambda.env;
  lm_gc (lm);
  ck_assert (!pool_gc_is_free (closure));
  ck_assert (!pool_gc_is_free (frame));
  run_eval_progn ("(set! escaped NIL)");
  lm_gc (lm);
  ck_assert (pool_gc_is_free (closure));
  ck_assert (pool_gc_is_free (frame));
}
END_TEST

START_TEST (test_invalid_forms_and_improper_lists)
{
  const char *invalid[] = { "(let x 1)",
                            "(let ((x . 1)) x)",
                            "(quote)",
                            "(quote a b)",
                            "(eval)",
                            "(eval 1 2)",
                            "(apply)",
                            "(funcall)",
                            "(map)",
                            "(+ 1 . 2)",
                            "(length '(a . b))",
                            "(reverse '(a . b))",
                            "(nth 0 '(a . b))",
                            "(last '(a . b))",
                            "(butlast '(a . b))",
                            "(append '(a) '(b . c))",
                            "(map list '(a . b))",
                            "(apply + '(1 . 2))" };
  for (size_t i = 0; i < sizeof (invalid) / sizeof (*invalid); ++i)
    ck_assert_msg (NILP (run_eval_progn (invalid[i])), "%s should fail",
                   invalid[i]);

  run_eval_progn ("(define changed 0)");
  ck_assert_ptr_eq (run_eval_progn ("(define bad 1 (set! changed 9))"), NIL);
  ck_assert_int_eq (run_eval_progn ("changed")->integer, 0);
  ck_assert_int_eq (run_eval_progn ("(+ 1 2)")->integer, 3);
}
END_TEST

START_TEST (test_formatter_pairs_and_float)
{
  const char *inputs[]   = { "'(a . b)", "'(a b . c)", "'(a b)", "42.5" };
  const char *expected[] = { "(a . b)", "(a b . c)", "(a b)", "42.500000" };
  for (size_t i = 0; i < 4; ++i)
    {
      char *formatted = format (run_eval_progn (inputs[i]));
      ck_assert_str_eq (formatted, expected[i]);
      free (formatted);
    }
}
END_TEST

START_TEST (test_parser_stream_lifecycle)
{
  for (int i = 1; i <= 2; ++i)
    {
      FILE *in = tmpfile ();
      ck_assert_ptr_nonnull (in);
      fprintf (in, "(+ %d 10)", i);
      rewind (in);
      Cell *ast = NULL;
      ck_assert (parser_stream (in, &ast, lm));
      ck_assert_int_eq (lm_progn (lm, ast)->integer, i + 10);
      fclose (in);
    }
  ck_assert_int_eq (run_eval_progn ("(+ 3 10)")->integer, 13);
  Cell *ast = T;
  ck_assert (!parser_stream (NULL, &ast, lm));
  ck_assert_ptr_eq (ast, NIL);
  ck_assert (!parser_buf ("(. x)", &ast, lm));
}
END_TEST

START_TEST (test_string_ownership_and_pool_boundary)
{
  char  original[] = "owned";
  Cell *string     = STRING (original, lm);
  original[0]      = 'X';
  ck_assert_str_eq (string->string, "owned");
  lm->stk.cells[lm->stk.sp++] = string;
  lm_gc (lm);
  ck_assert (!pool_gc_is_free (string));
  --lm->stk.sp;
  lm_gc (lm);
  ck_assert (pool_gc_is_free (string));

  Cell *last = NULL;
  while (lm->pool->free_list)
    last = INTEGER ((Integer)42, lm);
  lm_gc (lm);
  ck_assert (pool_gc_is_free (last));
  for (size_t i = 0; i < LM_CAP_PER_POOL + 1; ++i)
    STRING ("payload", lm);
  ck_assert_ptr_nonnull (lm->pool->next);
}
END_TEST

// START_TEST (test_string_literals)
// {
//   // if you support strings
//   Cell *r = run_eval_progn ("(print \"hello world\")");
//   ck_assert_ptr_eq (r, T);
// }
END_TEST

Suite *
eval_suite (void)
{
  Suite *s = suite_create ("Eval");

  TCase *tc = tcase_create ("Core");
  tcase_add_checked_fixture (tc, setup, teardown);

  tcase_add_test (tc, test_literal_expressions);
  tcase_add_test (tc, test_quote);
  tcase_add_test (tc, test_cons);
  tcase_add_test (tc, test_define_set_and_lookup);
  tcase_add_test (tc, test_first);
  tcase_add_test (tc, test_rest);
  tcase_add_test (tc, test_len);
  tcase_add_test (tc, test_if);
  tcase_add_test (tc, test_list);
  tcase_add_test (tc, test_lambda);
  tcase_add_test (tc, test_apply);
  tcase_add_test (tc, test_funcall);
  tcase_add_test (tc, test_eval);
  tcase_add_test (tc, test_last);
  tcase_add_test (tc, test_butlast);
  tcase_add_test (tc, test_map);
  tcase_add_test (tc, test_define);
  tcase_add_test (tc, test_let);
  tcase_add_test (tc, test_and_or);
  tcase_add_test (tc, test_progn);
  tcase_add_test (tc, test_map_edge_cases);
  tcase_add_test (tc, test_error_arity);
  tcase_add_test (tc, test_nested_let_lexical_scope);
  tcase_add_test (tc, test_invalid_let_bindings);
  tcase_add_test (tc, test_parser_errors);
  tcase_add_test (tc, test_gc_reclaims_unreachable_cells);
  tcase_add_test (tc, test_quote_edge);
  tcase_add_test (tc, test_closure_shared_locations);
  tcase_add_test (tc, test_gc_collects_closure_cycles);
  tcase_add_test (tc, test_invalid_forms_and_improper_lists);
  tcase_add_test (tc, test_formatter_pairs_and_float);
  tcase_add_test (tc, test_parser_stream_lifecycle);
  tcase_add_test (tc, test_string_ownership_and_pool_boundary);
  // tcase_add_test (tc, test_string_literals);

  suite_add_tcase (s, tc);
  return s;
}
