# lispm

A simple Lisp-dialect with parts from Scheme.

---

## Table of Contents

- [Quickstart](#quickstart)
- [REPL Usage](#repl-usage)
- [Types](#types)
- [Special Forms](#special-forms)
- [Primitives](#primitives)
  - [Literals](#literals)
  - [`quote`](#quote)
  - [`set`](#set)
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

### `set`

```lisp
(set symbol value)
```

Binds `symbol` to `value` in the global environment.

```lisp
(set 'x 10) ; ⇒ 10
x           ; ⇒ 10
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
(set 'add2 (lambda (x y) (+ x y)))
(add2 3 4) ; ⇒ 7
```

### `progn`

```lisp
(progn expr1 expr2 …)
```

Evaluate each `exprN` in sequence and return the last value.

```lisp
(progn
  (set 'x 5)
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

### `set`

See [Special Forms](#special-forms).

### `let`

See [Special Forms](#special-forms).

### `lambda`

See [Special Forms](#special-forms).

### `funcall` / `apply`

* **`funcall`** — call a function with individual args:

  ```lisp
  (funcall set 'x 100) ; ⇒ 100
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

* **`first`**, **`rest`**

  ```lisp
  (first '(a b)) ; ⇒ a
  (rest  '(a b)) ; ⇒ (b)
  ```

* **`length`**

  ```lisp
  (length '(1 2 3)) ; ⇒ 3
  ```

* **`reverse`**, **`butlast`**, **`last`**, **`nth`**, **`zip`**

  ```lisp
  (reverse '(1 2 3))   ; ⇒ (3 2 1)
  (butlast '(1 2 3))   ; ⇒ (1 2)
  (last '(1 2 3))      ; ⇒ (3)
  (nth 1 '(a b c))     ; ⇒ b
  (mapcar list '(A B) '(1 2)) ; ⇒ ((A 1) (B 2))
  ```

### Equality

* **`eq`**

  ```lisp
  (eq 'a 'a) ; ⇒ T
  (eq '(a) '(a)) ; ⇒ NIL  ; pointer equality, not deep
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

* [ ] `add` / `mul` with zero args should return `1`
* [ ] Proper exception objects
* [ ] Detailed README & documentation
* [ ] `define` form for globals
* [ ] Full support for strings
* [ ] Parse‐error reporting (exceptions)
* [ ] Garbage collection
* [ ] Maximum symbol length
* [ ] `let*`, `letrec`, `define`, `defun`
* [ ] Improved REPL (history, line editing)
