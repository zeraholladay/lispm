# lispm

A simple Scheme-dialect.

---

## Table of Contents

- [Quickstart](#quickstart)
- [REPL Usage](#repl-usage)
- [Types](#types)
- [Special Forms](#special-forms)
- [Primitives](#primitives)
  - [Literals](#literals)
  - [`quote`](#quote)
  - [`define`](#define)
  - [`set!`](#set)
  - [`let`](#let)
  - [`lambda`](#lambda)
  - [`funcall` / `apply`](#funcall--apply)
  - [`progn`](#progn)
  - [Conditionals](#conditionals)
  - [List Operations](#list-operations)
  - [Equality](#equality)
  - [Printing](#printing)
  - [Math](#math)
  - [`map`](#map)
- [TODO](#todo)

---

## Quickstart

Build:

```bash
make clean all
````

Run tests:

```bash
make clean test
```

Run the interpreter:

```bash
bin/lispm
```

Enable debug logging:

```bash
DEBUG=1 make clean all
```

---

## REPL Usage

Start the REPL:

```bash
bin/lispm
```

Type expressions at the prompt and press **Enter**.
Use `Ctrl+D` (EOF) to exit.

---

## Types

* **NIL** — the empty list / false
* **Symbols** — identifiers, e.g. `foo`
* **Integers** — signed numbers, e.g. `42`, `-7`
* **Primitive functions** — built in C
* **Lambda** — user-defined functions
* **List** — linked cons cells (`Cell`)

---

## Special Forms

### `quote`

```lisp
(quote expr)
'expr
```

Returns `expr` without evaluating it.

```lisp
'foo          ; ⇒ foo
(quote (1 2)) ; ⇒ (1 2)
```

### `define`

```lisp
(define symbol value)

(define (symbol zero-or-more-params …) body)
```

Binds `symbol` to `value` in the current lexical scope **or** creates a function (bound `lambda`) in the current lexical scope.

```lisp
(define 'x 10) ; ⇒ 10
x              ; ⇒ 10

(define (fib x)
  (if (or (< x 1) (eq x 1))
      x
      (let ((x (- x 1))
            (y (- x 2)))
          (+ (fib x) (fib y))
      )
  )
)

(fib 10) ; ⇒ 55
```

### `set!`

```lisp
(set! symbol value)
```

Evaluate `symbol`, then update the *existing* binding of `symbol` in the *innermost* (lexical) environment that defines it.  
Signal an error if `symbol` has not been previously bound.

```lisp
(set! x 10) ; ⇒ 10
x            ; ⇒ 10
```

### `let`

```lisp
(let ((var1 expr1)
      (var2 expr2)
      …)
  body1
  body2
  …)
```

1. Evaluate each `exprN` in the outer environment.
2. Bind `varN` to those values in a new lexical scope.
3. Evaluate the body forms in that scope.
4. Return the value of the last body form.

```lisp
(let ((a 1)
      (b 2)
      (c (+ a b)))  ; uses outer a/b
  (list a b c))    ; ⇒ (1 2 3)
```

Bindings must be two‐element lists `(name init-expr)`.

### `lambda`

```lisp
(lambda (p1 p2 … pN)
  body1
  body2
  …)
```

Creates an anonymous function closing over the current environment.

```lisp
(define add2 (lambda (x y) (+ x y))) ; or (define (add2 xy) (+ x y))
(add2 3 4) ; ⇒ 7
```

### `progn`

```lisp
(progn expr1 expr2 …)
```

Evaluate each `exprN` in sequence and return the last value.

```lisp
(progn
  (set! x 5)
  (* x 2)) ; ⇒ 10
```

---

## Primitives

### Literals

```lisp
NIL    ; ⇒ NIL (false)
T      ; ⇒ T   (true)
'foo   ; ⇒ foo (symbol)
42     ; ⇒ 42  (integer)
```

### `quote`

See [Special Forms](#special-forms).

### `set!`

See [Special Forms](#special-forms).

### `let`

See [Special Forms](#special-forms).

### `lambda`

See [Special Forms](#special-forms).

### `funcall` / `apply`

* **`funcall`** — call a function with individual args:

  ```lisp
  (funcall cons 'x 100) ; ⇒ (x.100)
  ```

* **`apply`** — call a function with a list of args:

  ```lisp
  (apply + '(1 2 3)) ; ⇒ 6
  ```

### `progn`

See [Special Forms](#special-forms).

### Conditionals

* **`if`**

  ```lisp
  (if cond then-expr else-expr)
  ```

* **`and`**, **`or`**

  ```lisp
  (and a b c)
  (or  a b c)
  ```

### List Operations

* **`cons`**

  ```lisp
  (cons x y) ; pair (x . y) or list if y is a list
  ```

* **`list`**

  ```lisp
  (list a b c) ; ⇒ (a b c)
  ```

* **`car`**, **`cdr`**

  ```lisp
  (car '(a b))  ; ⇒ a
  (cdr  '(a b)) ; ⇒ (b)
  ```

Alternatively, `first` and `rest`.

* **`length`**

  ```lisp
  (length '(1 2 3)) ; ⇒ 3
  ```

* **`reverse`**, **`butlast`**, **`last`**, **`nth`**

  ```lisp
  (reverse '(1 2 3))   ; ⇒ (3 2 1)
  (butlast '(1 2 3))   ; ⇒ (1 2)
  (last '(1 2 3))      ; ⇒ (3)
  (nth 1 '(a b c))     ; ⇒ b
  (map list '(A B) '(1 2)) ; ⇒ ((A 1) (B 2))
  ```

### Equality

* **`eq`**

  ```lisp
  (eq 'a 'a)      ; ⇒ T
  (eq '(a) '(a))  ; ⇒ NIL  ; pointer equality, not deep
  ```

### Printing

* **`print`**

  ```lisp
  (print 'hello) ; prints hello, returns T
  ```

### Math

* **`add`, `sub`, `mul`, `div`** (or `+ - * /`)

  ```lisp
  (add 1 2 3) ; ⇒ 6
  (+ 1 2 3)   ; ⇒ 6
  (sub 5 2)   ; ⇒ 3
  (* 3 4)     ; ⇒ 12
  (/ 8 2)     ; ⇒ 4
  ```

---

## TODO

* [ ] Full support for strings
* [ ] Parse‐error reporting (exceptions)
* [ ] Garbage collection
* [ ] Maximum symbol length
* [ ] `let*`, `letrec`, `define`

---

ChatGPT's list of Scheme functions (these are here for my reference):

## Special Forms

(These aren’t functions—you can’t rebind them.)

* **`(quote x)`** — prevent evaluation
* **`(lambda (…params…) …body…)`** — anonymous function
* **`(if test then else)`** — conditional
* **`(set! var expr)`** — mutate existing binding
* **`(define var expr)`** / **`(define (f args…) …)`** — top-level or internal binding
* **`(begin e1 e2 …)`** — sequence
* **`(cond (t1 e1 …) (t2 e2 …) … [else eN …])`** — multi-branch conditional
* **`(case key ((v1 v2) e1 …) … [else eN …])`** — branch by value
* **`(let ((v1 e1) …) body…)`** — local bindings
* **`(let* ((v1 e1) (v2 e2) …) body…)`** — sequential locals
* **`(letrec ((v1 e1) …) body…)`** — mutually recursive locals
* **`(do ((v1 init1 step1) …) (test result…) body…)`** — imperative loop
* **`(delay expr)`** / **`(force promise)`** — lazy evaluation
* **`` `… ,… ,@… ``** — quasiquotation

---

## Data‐structure Primitives

### Pairs & Lists

* **`(cons a d)`**, **`(car p)`**, **`(cdr p)`**, **`(set-car! p v)`**, **`(set-cdr! p v)`**
* **`(list e1 e2 …)`**, **`(null? x)`**, **`(pair? x)`**, **`(list? x)`**, **`(length lst)`**

### Vectors

* **`(make-vector n [init])`**, **`(vector-ref v i)`**, **`(vector-set! v i x)`**,
  **`(vector-length v)`**, **`(vector->list v)`**, **`(list->vector lst)`**

### Strings & Characters

* **`(string? s)`**, **`(string-length s)`**, **`(string-ref s i)`**, **`(string-set! s i ch)`**
* **`(string-append s1 s2 …)`**, **`(substring s start end)`**, **`(string->list s)`**,
  **`(list->string lst)`**, **`(char->integer ch)`**, **`(integer->char n)`**,
* **`(string=? s1 s2)`**, **`(string<? s1 s2)`**, etc.

---

## Type Predicates & Equality

* **Types**:
  `boolean?`, `number?`, `integer?`, `real?`, `rational?`, `complex?`,
  `exact?`, `inexact?`, `symbol?`, `string?`, `char?`, `vector?`, `port?`

* **Equality**:

  * **`eq?`** (pointer/symbol identity)
  * **`eqv?`** (numeric/string/boolean identity)
  * **`equal?`** (deep structural equality)

---

## Numeric Primitives

### Arithmetic

* **Integers & Rationals**:
  `+`, `-`, `*`, `/`, `quotient`, `remainder`, `modulo`,
  `numerator`, `denominator`, `gcd`, `lcm`
* **Reals & Complex**:
  `exp`, `log`, `sqrt`, `sin`, `cos`, `tan`, `atan`, `expt`,
  `real-part`, `imag-part`, `magnitude`, `angle`
* **Rounding**:
  `floor`, `ceiling`, `truncate`, `round`, `inexact->exact`, `exact->inexact`

### Comparisons

* Numeric: `=`, `<`, `>`, `<=`, `>=`
* Generic: `zero?`, `positive?`, `negative?`, `odd?`, `even?`

---

## Logical & Control

* **`(not x)`**, **`(and e1 e2 …)`**, **`(or e1 e2 …)`**
* **`(call/cc f)`** or **`(call-with-current-continuation f)`** — first-class continuations

---

## I/O & Ports

* **Basic**:
  `display`, `write`, `newline`, `read`, `read-char`, `peek-char`
* **File access**:
  `open-input-file`, `open-output-file`, `close-input-port`, `close-output-port`
* **With-resource**:
  `call-with-input-file`, `call-with-output-file`

---
