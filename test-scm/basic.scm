;;; lispm: an executable language tour
;;; =================================
;;;
;;; From the repository root:
;;;   make all
;;;   bin/lispm test-scm/basic.scm
;;;   bin/lispm -i test-scm/basic.scm  ; run, then explore the demo's bindings
;;;
;;; Every example below is checked, not merely printed. Successful checks emit
;;; (pass label actual-value); a mismatch raises an error containing the label,
;;; expected value, and actual value, and the interpreter exits unsuccessfully.
;;; A successful run ends with a summary and all-checks-passed.
;;;
;;; check is an ORDINARY function. Its actual argument is evaluated once before
;;; check runs; it does not quote/eval test expressions or mistake a truthy
;;; wrong answer for success. equal? handles numbers and structured results.
;;;
;;; This tour uses the prelude automatically loaded by the interpreter. It
;;; defines its own small example functions; those are demo code, not additional
;;; standard-library procedures. Names and prelude bindings are case-sensitive.
;;; There are no string literals yet, so labels and messages are quoted symbols.

(define checks-passed 0)
(define sections-run 0)

(define (section name)
  (set! sections-run (+ sections-run 1))
  (print (list 'section sections-run name)))

(define (check label expected actual)
  (if (equal? expected actual)
      (progn
        (set! checks-passed (+ checks-passed 1))
        (print (list 'pass label actual))
        actual)
      (error (list 'check-failed label
                   (list 'expected expected)
                   (list 'actual actual)))))

(print '(lispm executable-language-tour))

;;; 1. Values, quotation, and truth
;;; ------------------------------
;;; Only NIL is false; () is the same empty-list value. A quoted name is data,
;;; so 'NIL is a symbol, not false. Inside quoted lists, use () for empty data.

(section 'values-quotation-and-truth)
(check 'integer-literal 42 42)
(check 'float-literal 3.5 3.5)
(check 'quoted-symbol 'hello 'hello)
(check 'long-quote-syntax 'hello (quote hello))
(check 'quoted-expression '(+ 1 2) '(+ 1 2))
(check 'empty-list-is-nil T (eq NIL '()))
(check 'quoted-nil-is-a-symbol T (symbol? 'NIL))
(check 'quoted-nil-is-not-empty NIL (null? 'NIL))
(check 'empty-list-element '(a () c) (list 'a NIL 'c))
(check 'zero-is-truthy 'yes (if 0 'yes 'no))
(check 'empty-list-is-false 'no (if () 'yes 'no))
(check 'logical-not T (not NIL))
(check 'not-is-not-numeric-negation NIL (not 0))

;;; 2. Bindings and lexical scope
;;; -----------------------------
;;; define creates a mutable binding in the current frame; set! updates an
;;; existing binding. Neither form quotes its target name. let's initializers
;;; all see the OUTER frame, not earlier bindings from the same let.

(section 'bindings-and-lexical-scope)
(define answer 40)
(check 'define-value 40 answer)
(set! answer (+ answer 2))
(check 'set-existing-value 42 answer)
(define Answer 7)
(check 'ordinary-names-are-case-sensitive 49 (+ answer Answer))
(check 'parallel-let-initializers '(1 42)
       (let ((answer 1) (previous answer)) (list answer previous)))
(check 'let-does-not-rebind-global 42 answer)
(check 'set-nearest-frame 100
       (let ((answer 100))
         (let ((answer 5)) (set! answer 6))
         answer))
(check 'progn-sequences-in-current-scope 8
       (progn (define steps 3) (set! steps (+ steps 1)) (* steps 2)))

;;; 3. Control flow and short-circuiting
;;; -----------------------------------
;;; and/or return operand VALUES rather than normalizing every answer to T.
;;; The error calls in skipped branches are real guards: running one would
;;; abort the tour. or must also evaluate its successful operand only once.

(section 'control-flow)
(check 'if-skips-unused-branch 42 (if T 42 (error 'unreachable)))
(check 'if-without-alternative NIL (if NIL 42))
(check 'empty-progn NIL (progn))
(check 'empty-and T (and))
(check 'empty-or NIL (or))
(check 'and-returns-last-value 3 (and 1 2 3))
(define visits 0)
(check 'or-returns-zero 0
       (or NIL (progn (set! visits (+ visits 1)) 0) (error 'unreachable)))
(check 'or-evaluates-success-once 1 visits)
(check 'and-short-circuits NIL (and NIL (set! visits 99)))
(check 'skipped-side-effect 1 visits)

;;; 4. Native numbers and arithmetic conventions
;;; --------------------------------------------
;;; Integer division truncates toward zero. A float promotes the running
;;; result when it is encountered, not retroactively. Numeric = handles both
;;; number types; it does not round large integers into equality with floats.

(section 'numbers-and-arithmetic)
(check 'empty-sum 0 (+))
(check 'empty-product 1 (*))
(check 'sum 6 (+ 1 2 3))
(check 'native-name-alias 9 (add 4 5))
(check 'native-lookup-is-case-insensitive 3 (ADD 1 2))
(check 'subtraction 5 (- 10 2 3))
(check 'unary-negation -5 (sub 5))
(check 'product 24 (mul 2 3 4))
(check 'integer-division 4 (div 9 2))
(check 'division-truncates-toward-zero -4 (/ -9 2))
(check 'unary-division-is-not-reciprocal 42 (/ 42))
(check 'float-division 2.5 (/ 5 2.0))
(check 'promotion-is-not-retroactive (list 2.0 NIL)
       (let ((value (/ 5 2 1.0))) (list value (integer? value))))
(check 'ascending-comparison T (lt 1 2 3))
(check 'descending-comparison T (gt 3 2 1))
(check 'mixed-number-equality T (= 2 2.0 2))
(check 'large-integers-remain-distinct NIL
       (= 9007199254740993 9007199254740992.0))
(check 'numeric-prelude-predicates (list T T T T T T)
       (list (null? NIL) (zero? 0.0) (positive? 1.5) (negative? -2)
             (<= 2 2.0) (>= 3 2)))

;;; 5. Pairs, runtime types, and equality
;;; ------------------------------------
;;; cons constructs a pair. Its cdr may be an atom, making a dotted pair.
;;; eq distinguishes object identity from equal?'s structural comparison.
;;; Use = (not eq) to compare floating-point numbers.

(section 'pairs-types-and-equality)
(define colors (list 'red 'green 'blue))
(define shared (list 1 2))
(check 'construct-dotted-pair '(name . alice) (cons 'name 'alice))
(check 'pair-car 'name (car '(name . alice)))
(check 'pair-cdr 'alice (cdr '(name . alice)))
(check 'car-of-empty-list NIL (car NIL))
(check 'cdr-of-empty-list NIL (cdr NIL))
(check 'native-type-predicates (list T NIL T T NIL T)
       (list (pair? shared) (pair? NIL) (number? 3.5) (integer? 3)
             (integer? 3.0) (symbol? 'red)))
(check 'same-object T (eq shared shared))
(check 'different-list-objects NIL (eq shared (list 1 2)))
(check 'equal-list-contents T (equal? shared '(1.0 2.0)))
(check 'interned-symbol-identity T (eq 'red 'red))
(check 'eq-is-type-sensitive NIL (eq 2 2.0))
(check 'native-aliases-share-identity T (eq + add))

;;; 6. The Scheme list toolbox
;;; -------------------------
;;; These operations are prelude procedures, not native evaluator cases.
;;; nth is zero-based; last returns a one-element list, even for empty input.
;;; reverse/butlast copy list spines; append shares its final input's spine.

(section 'prelude-list-toolbox)
(check 'first-alias 'red (first colors))
(check 'rest-alias '(green blue) (rest colors))
(check 'caar-accessor 'a (caar '((a b) c)))
(check 'cadr-accessor 'green (cadr colors))
(check 'cdar-accessor '(b) (cdar '((a b) c)))
(check 'cddr-accessor '(blue) (cddr colors))
(check 'caddr-accessor 'blue (caddr colors))
(check 'length 3 (length colors))
(check 'empty-length 0 (length NIL))
(check 'reverse '(blue green red) (reverse colors))
(check 'indexed-element 'green (nth 1 colors))
(check 'out-of-range-index NIL (nth 99 colors))
(check 'negative-index NIL (nth -1 colors))
(check 'last-is-a-list '(blue) (last colors))
(check 'last-empty-convention '(()) (last NIL))
(check 'butlast '(red green) (butlast colors))
(check 'butlast-singleton NIL (butlast '(red)))
(check 'proper-list-shape T (list? colors))
(check 'dotted-pair-is-not-a-list NIL (list? '(a . b)))
(define tail (list 'blue 'violet))
(define joined (append '(red green) tail))
(check 'append-values '(red green blue violet) joined)
(check 'append-shares-final-tail T (eq (cddr joined) tail))
(check 'empty-append NIL (append))
(check 'source-list-was-not-mutated '(red green blue) colors)

;;; 7. Functions, rest parameters, apply, and funcall
;;; -----------------------------------------------
;;; A proper parameter list has fixed arity. A bare symbol receives all values;
;;; a dotted parameter list receives required values followed by a rest list.
;;; first/rest can be local parameter names now that they are prelude aliases.

(section 'functions-and-variadic-arguments)
(define (square x) (* x x))
(define (collect . values) values)
(define (tag head . extras) (cons head extras))
(check 'fixed-arity-function 49 (square 7))
(check 'anonymous-function 43 ((lambda (x) (+ x 1)) 42))
(check 'rest-only-empty NIL (collect))
(check 'rest-only-values '(1 a ()) (collect 1 'a NIL))
(check 'required-with-no-extras '(item) (tag 'item))
(check 'required-and-rest '(item 1 2) (tag 'item 1 2))
(check 'anonymous-rest-only '(1 2 3) ((lambda values values) 1 2 3))
(check 'aliases-can-be-parameter-names '(1 2 3)
       ((lambda (first . rest) (cons first rest)) 1 2 3))
(check 'scheme-procedure-type T (procedure? square))
(check 'native-procedure-type T (procedure? +))
(check 'apply-list-of-values 6 (apply + '(1 2 3)))
(check 'apply-fixed-prefix 42 (apply + 10 '(20 12)))
(check 'apply-variadic-procedure '(a 1 2) (apply tag 'a '(1 2)))
(check 'funcall 42 (funcall (lambda () (+ 40 2))))

;;; 8. Closures and shared mutable state
;;; -----------------------------------
;;; Each factory invocation gets its own environment. Returned closures keep
;;; that environment alive; sibling closures from one factory share bindings.

(section 'closures-and-state)
(define (make-adder amount) (lambda (x) (+ amount x)))
(define add-ten (make-adder 10))
(check 'returned-function 15 (add-ten 5))
(check 'lexical-not-dynamic-scope 15 (let ((amount 1000)) (add-ten 5)))
(define (make-counter start)
  (lambda () (set! start (+ start 1)) start))
(define counter-a (make-counter 0))
(define counter-b (make-counter 10))
(check 'counter-first-call 1 (counter-a))
(check 'counter-retains-mutation 2 (counter-a))
(check 'factory-instances-are-independent 11 (counter-b))
(check 'first-counter-still-independent 3 (counter-a))
(check 'captured-parameter-beats-caller-binding 4
       (let ((start 999)) (counter-a)))
(define siblings
  (let ((value 0))
    (list (lambda () (set! value (+ value 1))) (lambda () value))))
(check 'sibling-writer 1 (funcall (first siblings)))
(check 'sibling-reader-shares-binding 1 (funcall (cadr siblings)))

;;; 9. Higher-order functions and mapping
;;; ------------------------------------
;;; Functions are values: pass them, return them, compose them, and apply them.
;;; map takes one or more lists and stops at the shortest input.

(section 'higher-order-functions)
(define numbers '(1 2 3 4 5))
(define distance-from-ten (compose abs (lambda (x) (- x 10))))
(check 'identity-keeps-the-object T (eq (identity colors) colors))
(check 'composition 7 (distance-from-ten 3))
(check 'constant-function '(7 7 7) (map (constantly 7) colors))
(check 'map-one-list '(1 4 9 16 25) (map square numbers))
(check 'map-shortest-list '((red 10) (green 20))
       (map list colors '(10 20)))
(check 'map-variadic-callback '((a 1 x) (b 2 y))
       (map collect '(a b) '(1 2) '(x y)))
(check 'map-through-apply '(11 22)
       (apply map (list + '(1 2) '(10 20))))
(check 'composed-pipeline '(7 0 4) (map distance-from-ten '(3 10 14)))

;;; 10. Folds and ordered side effects
;;; ---------------------------------
;;; fold-left supplies (accumulator element) and is tail-recursive. fold-right
;;; supplies (element folded-suffix), associates from the right, and is not
;;; tail-recursive. for-each discards callback values and returns T.

(section 'folds-and-side-effects)
(check 'left-fold-sum 15 (fold-left + 0 numbers))
(check 'left-fold-order -6 (fold-left - 0 '(1 2 3)))
(check 'right-fold-order 2 (fold-right - 0 '(1 2 3)))
(check 'right-fold-builds-list '(1 2 3) (fold-right cons NIL '(1 2 3)))
(check 'empty-fold-keeps-initial 99 (fold-left + 99 NIL))
(define visit-total 0)
(define visit-order NIL)
(check 'for-each-result T
       (for-each (lambda (x)
                   (set! visit-total (+ visit-total x))
                   (set! visit-order (cons x visit-order)))
                 '(1 2 3)))
(check 'for-each-total 6 visit-total)
(check 'for-each-visits-left-to-right '(1 2 3) (reverse visit-order))

;;; 11. Filtering and short-circuit searches
;;; ---------------------------------------
;;; find returns an ELEMENT; any?/every? return T or NIL. A matching NIL element
;;; cannot be distinguished from no match by find alone, but any? can tell us.

(section 'selection-and-search)
(define samples '(-3 0 4 -1 7))
(check 'filter-matching-elements '(4 7) (filter positive? samples))
(check 'find-first-match -3 (find negative? samples))
(check 'find-no-match NIL (find negative? numbers))
(check 'any-match T (any? zero? samples))
(check 'every-match T (every? positive? numbers))
(check 'any-empty-list NIL (any? positive? NIL))
(check 'every-empty-list T (every? positive? NIL))
(check 'find-a-nil-element NIL (find null? '(1 () 3)))
(check 'nil-element-really-was-found T (any? null? '(1 () 3)))
(check 'predicate-zero-is-truthy T (every? (constantly 0) numbers))

;;; 12. Membership and association lists
;;; -----------------------------------
;;; memq returns the original matching SUFFIX; assq returns the matching PAIR.
;;; Both use eq, so use find with numeric = when searching floating values.

(section 'membership-and-association-lists)
(define profile '((name . alice) (age . 42) (roles editor tester)))
(check 'member-suffix '(green blue) (memq 'green colors))
(check 'member-shares-original-suffix T (eq (memq 'green colors) (cdr colors)))
(check 'missing-member NIL (memq 'purple colors))
(check 'nil-member-is-representable '(() c) (memq NIL '(a () c)))
(check 'association-pair '(name . alice) (assq 'name profile))
(check 'association-value 42 (cdr (assq 'age profile)))
(check 'association-list-value '(editor tester) (cdr (assq 'roles profile)))
(check 'numeric-search 2 (find (lambda (x) (= x 2.0)) numbers))

;;; 13. A small application: query records and construct a report
;;; -----------------------------------------------------------
;;; Records are association lists. Compose filter, map, apply, lexical helpers,
;;; and floating-point division to select a team and summarize its scores.

(section 'record-query-application)
(define records
  '(((name . ada)   (team . blue) (score . 9))
    ((name . grace) (team . red)  (score . 7))
    ((name . linus) (team . blue) (score . 5))
    ((name . alan)  (team . blue) (score . 8))))
(define (field key record)
  (let ((entry (assq key record)))
    (if entry (cdr entry) (error (list 'missing-field key)))))
(define qualified
  (filter (lambda (record)
            (and (eq (field 'team record) 'blue)
                 (>= (field 'score record) 8)))
          records))
(define (report rows)
  (let ((scores (map (lambda (record) (field 'score record)) rows)))
    (define total (apply + scores))
    (list (cons 'names (map (lambda (record) (field 'name record)) rows))
          (cons 'total total)
          (cons 'average (if (null? scores)
                             NIL
                             (/ total (* 1.0 (length scores))))))))
(check 'record-field 'ada (field 'name (first records)))
(check 'selected-records '(ada alan)
       (map (lambda (record) (field 'name record)) qualified))
(check 'query-report '((names ada alan) (total . 17) (average . 8.5))
       (report qualified))
(check 'empty-report '((names) (total . 0) (average)) (report NIL))

;;; 14. Code as data, eval, and formatting
;;; ------------------------------------
;;; eval is explicit here, not hidden in the test helper. It uses the current
;;; lexical environment; lowering control syntax does not mutate quoted data.
;;; string formats a value into a runtime string; it is not reader syntax.

(section 'code-as-data-and-formatting)
(define expression (list '+ 40 2))
(check 'build-an-expression '(+ 40 2) expression)
(check 'evaluate-built-expression 42 (eval expression))
(check 'eval-sees-lexical-bindings 12 (let ((x 7)) (eval '(+ x 5))))
(define control-code '(or NIL (let ((n 6)) (* n 7))))
(check 'evaluate-derived-syntax 42 (eval control-code))
(check 'eval-keeps-source-data T
       (equal? control-code '(or NIL (let ((n 6)) (* n 7)))))
(check 'format-value-to-string (string 42) (string (+ 40 2)))
(check 'formatted-number-is-not-a-number NIL (number? (string 42)))
(check 'formatted-code-is-not-a-pair NIL (pair? (string expression)))

;;; 15. Ordinary recursion and proper tail calls
;;; -------------------------------------------
;;; factorial has multiplication left to perform after recursion returns, so
;;; it is NOT tail-recursive. The other examples put the next call last.
;;; Keep the non-tail case small; the deep cases demonstrate constant return-
;;; continuation usage rather than benchmarking an exponential expression tree.

(section 'recursion-and-tail-calls)
(define (factorial n)
  (if (zero? n) 1 (* n (factorial (- n 1)))))
(check 'ordinary-recursion 720 (factorial 6))
(define (fib n)
  (define (walk remaining a b)
    (if (zero? remaining) a (walk (- remaining 1) b (+ a b))))
  (walk n 0 1))
(check 'linear-tail-recursive-fibonacci 6765 (fib 20))
(define (countdown n acc)
  (if (zero? n) acc (countdown (- n 1) (+ acc 1))))
(check 'thirty-thousand-tail-calls 30000 (countdown 30000 0))
(define (even? n) (if (zero? n) T (odd? (- n 1))))
(define (odd? n) (if (zero? n) NIL (even? (- n 1))))
(check 'mutual-tail-recursion-even T (even? 6000))
(check 'mutual-tail-recursion-odd T (odd? 6001))
(define (relay n . values)
  (if (zero? n) values (apply relay (- n 1) values)))
(check 'variadic-tail-calls-through-apply '(a b c) (relay 10000 'a 'b 'c))

;;; 16. Generated data and automatic memory management
;;; --------------------------------------------------
;;; range is a demo-defined INCLUSIVE range. Its walker and the prelude's
;;; left folds are tail-recursive. The VM collects unreachable temporary cells
;;; automatically; there is no Scheme-level manual free or GC call to perform.

(section 'generated-data-and-garbage-collection)
(define (range from through)
  (define (walk current reversed)
    (if (> current through)
        (reverse reversed)
        (walk (+ current 1) (cons current reversed))))
  (walk from NIL))
(check 'inclusive-range '(3 4 5 6) (range 3 6))
(check 'empty-range NIL (range 3 2))
(define generated (range 1 1000))
(check 'generated-list-length 1000 (length generated))
(check 'generated-list-sum 500500 (fold-left + 0 generated))
(check 'tail-of-filtered-generated-data '(991 992 993 994 995 996 997 998 999 1000)
       (filter (lambda (n) (> n 990)) generated))
(check 'mapped-generated-data 333833500 (fold-left + 0 (map square generated)))
;; The closures from section 8 must still retain their frames after the long
;; computations and the automatic collections they triggered.
(check 'captured-state-survives-long-run 5 (counter-a))
(check 'captured-value-survives-long-run 15 (add-ten 5))

(print (list 'summary (list 'sections sections-run)
                     (list 'checks-passed checks-passed)))
(print 'all-checks-passed)

;;; Error examples to try SEPARATELY in the REPL:
;;;   (error 'deliberate-user-error)
;;;   (/ 1 0)
;;;   (set! never-defined 1)
;;;   ((lambda (x) x) 1 2)
;;;   (length '(a . b))
;;;   9223372036854775808
;;; They are not executed in this successful tour. Syntax/range errors reject
;;; the input during parsing; runtime errors stop the current evaluation and
;;; preserve earlier side effects. In file mode, either kind exits nonzero.
