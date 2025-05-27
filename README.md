# lispm: A Lisp-dialect.

## Build and Test

Build:

```bash
make clean all
```

Test:

```bash
make clean test
```

Debug:

```
env DEBUG=1 make clean test all
```

## Types

- NIL
- Symbols (i.e. a variable name)
- Integers
- Lambda
- Primitive functions (C functions)
- List (i.e. a Node)

## Primitives (or Operators)

### `T, NIL, symbols, and integers`

Examples:

```lisp
T    ; primitive symbol true
NIL  ; primitive NIL false
'foo ; symbol foo
'bar ; symbol bar
42   ; number 42
-42  ; number -42
```

---

### `QUOTE x`
Returns the unevaluated expressioin.

Examples:

```lisp
'foo            ; foo
(quote foo)     ; foo
'(foo)          ; (foo)
(quote '(foo))  ; (QUOTE foo)
```

---

### `SET x y`
Sets a symbol to a value in the current context.

Examples:

```lisp
(set 'foo 42) ; 42
foo           ; 42
```

---

### `CONS x y`
Constructs a list from two arguments.

Examples:

```lisp
(cons 'foo '(bar))  ; (foo bar)
(cons 'foo 'bar)    ; (foo.bar)
```

---

### `LIST arg1 arg2 ... argN`
Creates a proper list (one ending in NIL cell).

Examples:

```lisp
(list 'foo '(bar) 42) ; (foo (bar) 42)
```

---

### `FIRST x & REST x`
Returns the first or the rest of a list.

Examples:

```lisp
(first '(foo bar))  ; foo
(rest '(foo bar))   ; (bar)
```

---

### `LAMBDA (p1 p2 ... pN) ...`
Create and returns a lambda from a function `body` and its captured environment (lexical scope).

Examples:

```lisp
(lambda (a b) (cons a (cons b '()))) ; creates an anonymous lambda

(set 'foofn
      (lambda (a b c)
              (cons a (cons b (cons c '())))
      )
)
(foofn 1 2 '3)  ; call a lambda named foofn

((lambda (a b c)
  (cons a (cons b (cons c '()))))
  1 2 3) ; call an anonymous lambda with args 1 2 3
```

---

### `APPLY fn arglist`
Applies arguments to a primitive function or lambda.

Examples:

```lisp
(apply set '(a 42))
(apply first '((a 42))) ; a
(apply rest '((a 42)))  ; (42)
```

---

### `FUNCALL fn arg1 arg2 ... argN`
Calls arguments to a function.

Examples:

```lisp
(funcall set 'a 42)     ; a is 42
(funcall first '(a 42)) ; a
;
```

---

### `LEN x`
Length of a list.

Examples:

```lisp
(len '(1 2 3 4 5))
; 5
```
---

### `PAIR x y`
Pairs two lists:

Examples:

```lisp
(pair '(1 2 3 4 5)
      '(a b c d e))
```

---

### `EVAL x`
Evaluates an expression:

Examples:

```lisp
(set 'a 42)
(eval ''a)                  ; 42
(eval '(cons 'foo '(bar)))  ; (foo bar)
```

---

### `EQ`
Returns `T` or `NIL` if arguments are equal:

Examples:

```lisp
(eq T T)            ; T/true
(eq T NIL)          ; NIL/false
(eq '() '())        ; T
(eq '(dog) '(dog))  ; NIL
(eq (len '(a b c))
    (len '(1 2 3))) ; T
```

---

### `PRINT x`
Prints an argument:

Examples:

```lisp
(print 'foo) ; foo and returns T
```

---

### `MATH arg1 arg2 ... argN`
Basic math `ADD, SUB, MUL,` and `DIV`.

Examples:

```lisp
(ADD 10 11 10 11) ; 42
(+ 10 11 10 11)   ; 42
(- 43 1)          ; 42
(* 7 6)           ; 42
(/ 84 / 2)        ; 42
```

---

## TODO

In no particular order:

1. Internal call stack.
1. Environment should be methods on Ctx.
1. Environment should be a hashed dictionary and not a tree.
1. Generic type-based allocator
1. Add and mul should return 1 when no args.
1. Real exceptions
1. Memory management (tracking)
1. I/O subsystem (printf is ugly)
1. Docs/README.md
1. Test heaplist
1. Define/def
1. Strings (started kind of)
1. Fix yyarse after pool forever.
1. Parse errors (exceptions first)
1. GC
1. Max symbol size
1. Rename `set` to `let` & add `define`
1. `map-car`
1. cmake
