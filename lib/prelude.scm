;;; lispm's Scheme prelude
;;; =====================
;;;
;;; This file is ordinary Scheme source, embedded in the executable at build
;;; time and evaluated once in each new interpreter's global environment.
;;; You can read these definitions as examples, edit them, rebuild, and use the
;;; results immediately in either the REPL or a program file.
;;;
;;; These names are normal bindings, not new keywords. Users may redefine them
;;; with define or change them with set!. The native core supplies arithmetic,
;;; pairs, application, type predicates, numeric =, and error; this file builds
;;; the higher-level library from those pieces.
;;; Like other ordinary bindings, these names are case-sensitive. Use lowercase
;;; not, length, reverse, nth, last, butlast, list, append, and map, even though
;;; their former native registrations accepted uppercase/mixed-case spellings.
;;;
;;; Dialect conventions to keep in mind:
;;;   * NIL is both false and the empty list. Every other value is true,
;;;     including the number 0. Predicates here deliberately return T or NIL.
;;;     Inside quoted data, write () for an empty-list element: '(a () c).
;;;     The name NIL is normally resolved by evaluation; 'NIL is a symbol.
;;;   * Lambda parameters can be fixed (x y), dotted (x . extras), or a bare
;;;     symbol args that receives all arguments. Named define uses the same
;;;     dotted syntax, e.g. (define (collect . args) args). Names must be
;;;     distinct and cannot be protected native names such as first or rest.
;;;     compose and constantly intentionally remain unary helpers below.
;;;   * The reader has no string literals yet. error accepts a quoted symbol
;;;     (or other value), so we use symbols such as 'expected-proper-list.
;;;   * A tail call returns another call's result directly. Those calls use
;;;     bounded VM control-stack space. They may still allocate heap values.

;;; Small predicates and numeric conveniences
;;; ---------------------------------------

;; list demonstrates the smallest variadic procedure: no arguments are
;; required, and the binder puts all evaluated values into values in order.
;; This also handles (list) => NIL, and works through apply and funcall.
;; There is no recursive call or dependency on any other prelude procedure.
(define (list . values)
  values)

;; not only tests falsehood; it does not negate a number. Only NIL is false,
;; so (not 0) and (not '(a)) are both NIL, while (not NIL) is T.
;; Define it before list?, whose shape-checking walker uses it. Using the native
;; eq directly avoids a bootstrap dependency on any other prelude procedure.
(define (not value)
  (eq value NIL))

;; null? is true only of the empty list. In particular, (null? '(NIL)) is NIL:
;; a nonempty list containing NIL is still a pair, not an empty list.
(define (null? value)
  (eq value NIL))

;; Use numeric = rather than eq, so both (zero? 0) and (zero? 0.0) are true.
;; Nonnumeric arguments are rejected by the native numeric procedures.
(define (zero? value)
  (= value 0))

(define (positive? value)
  (> value 0))

(define (negative? value)
  (< value 0))

;; These inclusive comparisons intentionally take exactly two arguments.
;; The native < and > also accept chains; those are a separate interface.
(define (<= left right)
  (or (< left right) (= left right)))

(define (>= left right)
  (or (> left right) (= left right)))

;; abs uses the existing subtraction/overflow rules. For example, the absolute
;; value of the most negative integer cannot fit in the integer type and errors.
(define (abs value)
  (if (< value 0)
      (- value)
      value))

;;; Pair accessors
;;; --------------
;; Read the letters between c and r from RIGHT TO LEFT: cadr means "cdr, then
;; car". A pair need not be a proper list; these also work on suitable dotted
;; pairs. They inherit car/cdr's checks when an intermediate value is an atom.

(define (caar value)
  (car (car value)))

(define (cadr value)
  (car (cdr value)))

(define (cdar value)
  (cdr (car value)))

(define (cddr value)
  (cdr (cdr value)))

(define (caddr value)
  (car (cdr (cdr value))))

;;; Shape checking and structural equality
;;; --------------------------------------

;; list? distinguishes proper lists, which end in NIL, from dotted pairs,
;; which end in another atom. It also rejects cycles instead of looping.
;;
;; The local walk uses the "tortoise and hare" algorithm: slow moves one cdr
;; per iteration, fast moves two. Reaching NIL proves the list is proper;
;; reaching another atom proves it is improper; meeting again proves a cycle.
;; Only the cdr chain matters: a list may contain arbitrary values as elements.
(define (list? value)
  (define (walk slow fast)
    (if (null? fast)
        T
        (if (not (pair? fast))
            NIL
            (let ((next (cdr fast)))
              (if (null? next)
                  T
                  (if (not (pair? next))
                      NIL
                      (let ((slow-next (cdr slow))
                            (fast-next (cdr next)))
                        (if (eq slow-next fast-next)
                            NIL
                            (walk slow-next fast-next)))))))))
  (walk value value))

;; equal? compares acyclic pair structures by their contents, including dotted
;; pairs. Numeric leaves use =, so 2 and 2.0 compare equal. Other leaves use
;; eq's rules; procedure values, for example, compare by identity.
;;
;; Comparing the cars is NOT tail-recursive: the cdr comparison still remains.
;; The final cdr comparison is in tail position. Very deeply nested cars can
;; therefore still exhaust the control stack. Cyclic structures are not
;; supported by this equality procedure (unlike the cdr-cycle check in list?).
(define (equal? left right)
  (if (and (number? left) (number? right))
      (= left right)
      (if (pair? left)
          (and (pair? right)
               (equal? (car left) (car right))
               (equal? (cdr left) (cdr right)))
          (and (not (pair? right)) (eq left right)))))

;; These global helpers are private by naming convention only; this dialect
;; has no module system. Validate a public procedure's input ONCE, then let a
;; local walker traverse it without repeating the full list? check at every
;; recursive step. Rechecking each suffix would turn a linear walk quadratic.
(define (_prelude-require-list value)
  (if (list? value)
      T
      (error 'expected-proper-list)))

(define (_prelude-require-procedure value)
  (if (procedure? value)
      T
      (error 'expected-procedure)))

;;; Folds: reducing a list to one value
;;; ----------------------------------

;; fold-left calls f with the accumulator FIRST and the next element SECOND.
;; It visits elements from left to right and passes each result into the next
;; iteration. On an empty list it returns initial without calling f.
;;
;;   (fold-left - 0 '(1 2 3)) => ((0 - 1) - 2) - 3 => -6
;;
;; walk's recursive call is the final operation, so this uses bounded control
;; stack space even for long input lists. The accumulator itself may grow.
(define (fold-left f initial xs)
  (_prelude-require-procedure f)
  (_prelude-require-list xs)
  (define (walk acc remaining)
    (if (null? remaining)
        acc
        (walk (f acc (car remaining)) (cdr remaining))))
  (walk initial xs))

;; fold-right calls f with the element FIRST and the folded suffix SECOND.
;; It groups from the right, and f is called for the rightmost element first.
;; On an empty list it returns initial without calling f.
;;
;;   (fold-right cons NIL '(1 2 3)) => (cons 1 (cons 2 (cons 3 NIL)))
;;
;; Unlike fold-left, this straightforward definition is NOT tail-recursive:
;; each walk invocation must wait for the suffix before calling f. It is
;; useful for understanding right folds, but uses linear control-stack space.
(define (fold-right f initial xs)
  (_prelude-require-procedure f)
  (_prelude-require-list xs)
  (define (walk remaining)
    (if (null? remaining)
        initial
        (f (car remaining) (walk (cdr remaining)))))
  (walk xs))

;;; Basic list operations, implemented in Scheme rather than native thunks
;;; --------------------------------------------------------------------
;; The VM still has internal C list utilities for its own argument/control
;; bookkeeping. These LANGUAGE-LEVEL procedures are ordinary Scheme lambdas.
;; Their arities are enforced by lambda application, and the existing list
;; validator rejects atoms, improper lists, and cycles before traversal begins.

;; length counts one per element, ignoring the contents of the elements.
;; fold-left validates the input once and supplies tail-recursive traversal.
;; In particular, NIL has length 0 but a list containing NIL has length 1.
(define (length xs)
  (fold-left (lambda (count value) (+ count 1)) 0 xs))

;; reverse makes a fresh list spine by consing each element onto the front of
;; the accumulator. The element objects themselves are shared, not deep-copied.
;; This never mutates xs, even temporarily, and returns NIL for an empty list.
(define (reverse xs)
  (fold-left (lambda (reversed value) (cons value reversed)) NIL xs))

;; nth is ZERO-based: (nth 0 '(a b)) is a. Preserve the earlier convention of
;; returning NIL for a negative or out-of-range integer index. A non-integer
;; index is an error, even 1.0; the whole list is validated even if the index
;; would otherwise let us stop early. The selected element is returned as-is.
(define (nth index xs)
  (if (not (integer? index))
      (error 'expected-integer-index)
      T)
  (_prelude-require-list xs)
  (define (walk remaining position)
    (if (null? remaining)
        NIL
        (if (= position 0)
            (car remaining)
            (walk (cdr remaining) (- position 1)))))
  (if (< index 0)
      NIL
      (walk xs index)))

;; last returns a NEW ONE-ELEMENT LIST, not the final element or original tail.
;; Keep this dialect's historical empty-list behavior too: (last NIL) => (NIL).
;; The walker carries the most recently visited element, initially NIL. At the
;; end it wraps that value in a list, preserving object identity of the value.
(define (last xs)
  (_prelude-require-list xs)
  (define (walk remaining value)
    (if (null? remaining)
        (list value)
        (walk (cdr remaining) (car remaining))))
  (walk xs NIL))

;; butlast copies all elements except the final one. Empty and singleton inputs
;; both produce NIL. We accumulate in reverse order, then call reverse once to
;; restore the order. The input is validated once and reverse validates the
;; newly built copy; neither rechecks each suffix, so total work stays linear.
;; Like reverse, it shares elements but never changes the input's list spine.
(define (butlast xs)
  (_prelude-require-list xs)
  (define (walk remaining reversed)
    (if (or (null? remaining) (null? (cdr remaining)))
        (reverse reversed)
        (walk (cdr remaining) (cons (car remaining) reversed))))
  (walk xs NIL))

;;; Selecting, searching, and visiting
;;; ---------------------------------
;; The procedures in this section take one proper list and a unary procedure.
;; They validate the whole list before invoking user code. Within the walk,
;; predicates run once per visited element, in left-to-right order. A truthy
;; predicate result need not be the literal T; remember that even 0 is truthy.

;; filter returns a NEW list spine containing the selected original elements.
;; cons is efficient at the front, so the accumulator is reversed during the
;; walk. One final reverse restores input order. The input list is not changed.
(define (filter predicate xs)
  (_prelude-require-procedure predicate)
  (_prelude-require-list xs)
  (define (walk remaining reversed)
    (if (null? remaining)
        (reverse reversed)
        (if (predicate (car remaining))
            (walk (cdr remaining) (cons (car remaining) reversed))
            (walk (cdr remaining) reversed))))
  (walk xs NIL))

;; find stops at the first matching element and returns that ELEMENT, not a
;; list suffix. It returns NIL when nothing matches. This means a match whose
;; element is itself NIL is indistinguishable from no match; use any? when
;; only the existence of a match matters.
(define (find predicate xs)
  (_prelude-require-procedure predicate)
  (_prelude-require-list xs)
  (define (walk remaining)
    (if (null? remaining)
        NIL
        (if (predicate (car remaining))
            (car remaining)
            (walk (cdr remaining)))))
  (walk xs))

;; any? normalizes its result to T/NIL and short-circuits after the first
;; match. The empty list returns NIL: it contains no matching element.
(define (any? predicate xs)
  (_prelude-require-procedure predicate)
  (_prelude-require-list xs)
  (define (walk remaining)
    (if (null? remaining)
        NIL
        (if (predicate (car remaining))
            T
            (walk (cdr remaining)))))
  (walk xs))

;; every? short-circuits after the first failure. The empty list returns T:
;; there is no element that violates the predicate ("vacuous truth").
(define (every? predicate xs)
  (_prelude-require-procedure predicate)
  (_prelude-require-list xs)
  (define (walk remaining)
    (if (null? remaining)
        T
        (if (predicate (car remaining))
            (walk (cdr remaining))
            NIL)))
  (walk xs))

;; for-each runs f for its SIDE EFFECTS, discarding each return value. It
;; returns T after visiting all elements, including when xs is empty. Keeping
;; (walk ...) last makes this traversal tail-recursive despite f's effects.
(define (for-each f xs)
  (_prelude-require-procedure f)
  (_prelude-require-list xs)
  (define (walk remaining)
    (if (null? remaining)
        T
        (progn (f (car remaining)) (walk (cdr remaining)))))
  (walk xs))

;;; Variadic list operations
;;; ------------------------

;; append accepts zero or more PROPER lists; unlike some Scheme dialects, an
;; improper final argument is not allowed. All inputs are validated before
;; combining them. Zero arguments return NIL; one argument is returned as-is.
;;
;; Copy the elements of all but the final list into a reversed accumulator,
;; then cons them onto the final list. This preserves the old sharing contract:
;; the result has a fresh prefix but shares the last argument's list spine.
;; For example, (append '(a b) tail) ends with the very same tail object.
;;
;; Each input is traversed a constant number of times. The left folds and the
;; collect call are tail-recursive, so neither long lists nor many input lists
;; require linear control-stack space. No input list is mutated.
(define (append . lists)
  (for-each _prelude-require-list lists)
  (define (prepend-reversed reversed tail)
    (fold-left (lambda (acc value) (cons value acc)) tail reversed))
  (define (collect pending reversed)
    (if (null? (cdr pending))
        (prepend-reversed reversed (car pending))
        (collect (cdr pending)
                 (fold-left (lambda (acc value) (cons value acc))
                            reversed (car pending)))))
  (if (null? lists)
      NIL
      (collect lists NIL)))

;; map requires a procedure and AT LEAST ONE proper input list. The dotted
;; more-lists parameter captures any additional lists. We validate every list
;; before invoking f, even if some other list is empty, so malformed inputs do
;; not disappear behind shortest-list truncation.
;;
;; At each iteration, collect car from each input in argument order, apply f
;; to those VALUES, then continue with the cdr of every input. Stop as soon as
;; any input is empty. Calls to f occur left to right, once per result element.
;;
;; collect uses a left fold plus reverse instead of recursively building a
;; column on the return stack. walk likewise accumulates reversed results.
;; This keeps control-stack usage bounded as either input length or the number
;; of input lists grows. The input spines are never modified.
;;
;;   (map + '(1 2 3) '(10 20)) => (11 22)
;;   (apply map (list list '(a b) '(1 2))) => ((a 1) (b 2))
(define (map f first-list . more-lists)
  (_prelude-require-procedure f)
  (define inputs (cons first-list more-lists))
  (for-each _prelude-require-list inputs)
  (define (collect getter columns)
    (reverse (fold-left (lambda (acc xs) (cons (getter xs) acc))
                        NIL columns)))
  (define (walk columns reversed)
    (if (any? null? columns)
        (reverse reversed)
        (let ((arguments (collect car columns))
              (remaining (collect cdr columns)))
          (walk remaining (cons (apply f arguments) reversed)))))
  (walk inputs NIL))

;;; Lookup in lists and association lists
;;; ------------------------------------

;; memq uses eq, NOT structural equal?. On a match it returns the ORIGINAL
;; suffix beginning with that element; otherwise it returns NIL. Returning a
;; nonempty suffix lets callers distinguish finding NIL from finding nothing.
;;
;;   (memq 'b '(a b c)) => (b c)
;;   (memq NIL '(a () c)) => (NIL c)
;;
;; Symbols compare by their interned identity; pair elements must be the same
;; pair object, not merely have the same contents. Numeric behavior is inherited
;; from the native eq procedure; use find with = for numeric lookup.
(define (memq value xs)
  (_prelude-require-list xs)
  (define (walk remaining)
    (if (null? remaining)
        NIL
        (if (eq value (car remaining))
            remaining
            (walk (cdr remaining)))))
  (walk xs))

;; An association list is a proper list of pairs: ((name . alice) (age . 42)).
;; assq compares each pair's car to key using eq and returns the first matching
;; PAIR, not just its value. Use cdr on the result to obtain a dotted value.
;; Each visited entry must be a pair; the outer list is validated up front,
;; but entries after a match are not inspected because the search stops early.
(define (assq key entries)
  (_prelude-require-list entries)
  (define (walk remaining)
    (if (null? remaining)
        NIL
        (let ((entry (car remaining)))
          (if (not (pair? entry))
              (error 'expected-association-pair)
              (if (eq key (car entry))
                  entry
                  (walk (cdr remaining)))))))
  (walk entries))

;;; Small higher-order helpers
;;; --------------------------

;; identity returns the exact same value/object, without copying it.
(define (identity value)
  value)

;; constantly returns a unary closure. Its captured value survives the factory
;; call; its one argument is accepted but ignored. Example:
;;   (map (constantly 7) '(a b c)) => (7 7 7)
(define (constantly value)
  (lambda (ignored) value))

;; compose returns a unary closure for f(g(x)). The call to g is NOT tail
;; because f still needs its result; the call to f IS tail. Validation happens
;; when composing, rather than waiting for the resulting function to be called.
(define (compose f g)
  (_prelude-require-procedure f)
  (_prelude-require-procedure g)
  (lambda (value) (f (g value))))

;; The C bootstrapper requires this marker to confirm successful evaluation.
;; An error aborts before reaching it, so a partially loaded library cannot
;; accidentally be reported as a successful interpreter initialization.
T
