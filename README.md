# lispm

A small Scheme-like Lisp interpreter written in C. The C runtime implements
the reader, evaluation and binding rules, native procedures, and garbage
collection. A Scheme prelude supplies the higher-level library at startup.

This is its own dialect, not a complete implementation of a Scheme standard.
The reference below describes the operations implemented in this repository.

## Contents

- [Build and run](#build-and-run)
- [Core language: C-defined primitives and conventions](#core-language-c-defined-primitives-and-conventions)
  - [Reader, values, and truth](#reader-values-and-truth)
  - [Names, scope, and evaluation](#names-scope-and-evaluation)
  - [Binding and function forms](#binding-and-function-forms)
  - [Control forms](#control-forms)
  - [Application and evaluation](#application-and-evaluation)
  - [Native pair operations](#native-pair-operations)
  - [Native arithmetic and comparisons](#native-arithmetic-and-comparisons)
  - [Native equality and type predicates](#native-equality-and-type-predicates)
  - [Native output, strings, and errors](#native-output-strings-and-errors)
  - [Tail calls](#tail-calls)
- [Scheme prelude: additional startup procedures](#scheme-prelude-additional-startup-procedures)
  - [Logical and numeric helpers](#logical-and-numeric-helpers)
  - [Pair accessors and aliases](#pair-accessors-and-aliases)
  - [List construction and traversal](#list-construction-and-traversal)
  - [Folds](#folds)
  - [Selection, search, and effects](#selection-search-and-effects)
  - [Membership and association lists](#membership-and-association-lists)
  - [Higher-order helpers](#higher-order-helpers)
  - [Internal prelude helpers](#internal-prelude-helpers)
- [Implementation guide](#implementation-guide)
- [Current boundaries](#current-boundaries)

## Build and run

Dependencies: a C compiler, Make, Flex, Bison, gperf, `xxd`, Readline, and
`pkg-config`. Tests also require the Check C testing framework.

On macOS, select the Homebrew tools first:

```bash
source .env.sh
```

```bash
make all          # build bin/lispm
make test         # build and run the C regression tests
make lint         # format and check C code; also clears build/generated files
DEBUG=1 make all  # debug build
```

Header dependencies and compiler/flag changes trigger the necessary rebuilds.
An unchanged `make all` does not recompile or relink. Parallel builds such as
`make -j4 all test` are supported.

Start the REPL, run program files, or enter the REPL after running files:

```bash
bin/lispm
bin/lispm program.scm
bin/lispm -i program.scm
```

Run the self-checking language tour:

```bash
bin/lispm test-scm/basic.scm
bin/lispm -i test-scm/basic.scm  # run the tour, then explore its bindings
```

The demo walks through core forms, closures, variadic calls, the prelude, a
record-query application, and deep tail recursion. It prints named check
results and a final summary; a mismatch reports expected/actual values and
exits with failure status.

With `-i`, try `(report records)`, `(range 3 8)`, or `(counter-a)` afterward.

The REPL evaluates one submitted line at a time; use `Ctrl+D` to exit. Put the
multiline examples below in a file, or enter each complete expression on one
line. Program files can contain multiple expressions and multiline definitions.
File execution does not automatically print expression results; use `print`.

In examples, `; => ...` describes the result as the interpreter prints it.
The built-in true and false values are printed as `t` and `nil`.

## Core language: C-defined primitives and conventions

These facilities are implemented by the C reader, evaluator, syntax lowerer,
and native built-in registry. `let`, `and`, and `or` are C-supported derived
syntax, not procedures from the Scheme prelude.

### Reader, values, and truth

Expressions use prefix notation: `(procedure argument ...)`. A semicolon starts
a comment that continues to the end of the line.

| Value | Concrete example | Printed result / meaning |
| --- | --- | --- |
| Integer | `42` | `42` |
| Negative integer | `-7` | `-7` |
| Floating-point number | `3.5` | `3.500000` |
| Symbol as data | `'hello` | `hello` |
| Proper list as data | `'(1 2 3)` | `(1 2 3)` |
| Dotted pair as data | `'(a . b)` | `(a . b)` |
| Empty list / false | `NIL` or `()` | `nil` |
| True | `T` | `t` |

Only `NIL` is false. Every other value, including `0` and nonempty lists, is
truthy. Use `T`/`NIL`, not Scheme's `#t`/`#f` notation.

```lisp
(if 0 'yes 'no)    ; => yes
(if () 'yes 'no)   ; => no
```

Quoted names are not looked up. In particular, `'NIL` is a symbol, not the empty
list. To include an empty list inside quoted data, write `()`:

```lisp
NIL               ; => nil
'NIL              ; => NIL
'(a () c)         ; => (a nil c)
```

Integers use C `long long` (signed 64-bit on the current builds); floats use C
`double`. Literals accept decimal digits with an optional leading minus;
floating-point literals require digits on both sides of the decimal point.
Scientific notation, fractions, and complex numbers are not supported.
Literal overflow, reported floating-point underflow, and non-finite float
conversions are parse errors.

The runtime also has native procedure, Scheme lambda/closure, and string
values. Strings can be produced with `string`, but there is no string-literal
reader syntax yet.

### Names, scope, and evaluation

- Ordinary identifiers start with a letter or underscore and can contain
  letters, digits, `-`, `_`, `!`, and `?`. The reader also recognizes the
  supported arithmetic/comparison operator spellings.
- Native built-in lookup is case-insensitive. Ordinary variable and prelude
  bindings are case-sensitive.
- Native names and constants are protected from rebinding. User-defined names
  and prelude names may be changed with `define` or `set!`.
- Bindings are lexical. A closure retains its defining environment, including
  shared mutable bindings, rather than copying their values.
- An ordinary call evaluates its operator first, then arguments left to right.
  The explicit core forms `apply` and `funcall` currently evaluate their
  argument expressions left to right **before** their procedure expression.

```lisp
(define answer 42)
(define Answer 7)
(+ answer Answer) ; => 49
(ADD 1 2)         ; => 3
```

### Binding and function forms

#### `quote` and `'`

`(quote expression)` returns data without evaluating it. `'expression` is the
reader shorthand. Exactly one operand is required.

```lisp
(quote (+ 1 2)) ; => (+ 1 2)
'hello         ; => hello
```

#### `define`

`(define name expression)` evaluates the expression and creates or replaces a
binding in the current lexical frame. Do **not** quote the binding name.
Function-definition shorthand is also supported.

```lisp
(define answer (+ 40 2))
answer                    ; => 42
(define (square x) (* x x))
(square 5)                ; => 25
```

A literal value does not make its binding immutable: `(define limit 100)`
creates a mutable binding. There is no user-defined constant declaration.

#### `set!`

`(set! name expression)` evaluates the expression and replaces the nearest
existing lexical binding. It errors if the name has not been defined. The name
itself is not evaluated or quoted. Use the exclamation mark: `set` is not an
alias for this form.

```lisp
(define count 0)
(set! count (+ count 1)) ; => 1
count                   ; => 1
```

#### `lambda`

`(lambda parameters body ...)` creates a closure. Body expressions execute in
order and the final value is returned; an empty body returns `NIL`.

Three parameter shapes are supported:

```lisp
((lambda (x y) (+ x y)) 20 22)       ; => 42
((lambda args args) 1 2 3)           ; => (1 2 3)
((lambda (head . extras) extras) 1 2 3) ; => (2 3)
```

A proper parameter list requires exactly that many arguments. A bare symbol
receives all arguments, possibly `NIL`. A dotted list requires its fixed
parameters and binds the remaining values to its final symbol.

Named definitions support the same rest-argument convention:

```lisp
(define (collect . args) args)
(collect)       ; => nil
(collect 1 2 3) ; => (1 2 3)
```

Parameter names must be distinct, including the rest name, and cannot be
protected native names such as `car` or `cdr`. Malformed and duplicate
parameters are rejected during parsing. Rest arguments preserve evaluation
order; a rest binding reached through `apply` may share the supplied list suffix.

Closures retain binding locations, so mutation survives function returns:

```lisp
(define (make-counter start)
  (lambda () (set! start (+ start 1)) start))
(define counter (make-counter 10))
(counter) ; => 11
(counter) ; => 12
```

### Control forms

#### `if`

`(if test consequent [alternative])` evaluates only the selected branch. A
missing alternative produces `NIL` when the test is false.

```lisp
(if T 42 (error 'not-run)) ; => 42
(if NIL 42)               ; => nil
```

#### `progn`

`(progn expression ...)` evaluates expressions in order in the current scope
and returns the last result. With no expressions it returns `NIL`.

```lisp
(progn (define n 3) (set! n (+ n 1)) (* n 2)) ; => 8
(progn)                                     ; => nil
```

#### `let` — C-supported derived syntax

`(let ((name initializer) ...) body ...)` creates a lexical frame. Each binding
must contain exactly a name and an initializer. All initializers execute left
to right in the **outer** environment; bindings become visible in the body.
At least one body expression is required.

```lisp
(define x 10)
(let ((x 1) (y x)) (+ x y)) ; => 11
x                          ; => 10
```

The C lowerer turns `let` into a lambda application. Unlike lambda parameter
lists, duplicate `let` binding names are currently accepted; the last binding
wins. Prefer distinct names.

#### `and` — C-supported derived syntax

`(and expression ...)` stops at the first `NIL`; otherwise it returns the final
operand's actual value. With no operands it returns `T`.

```lisp
(and 1 2 3)                ; => 3
(and NIL (error 'not-run)) ; => nil
(and)                      ; => t
```

#### `or` — C-supported derived syntax

`(or expression ...)` returns the first non-`NIL` value, evaluating each visited
operand once. With no operands it returns `NIL`.

```lisp
(or NIL 0 (error 'not-run)) ; => 0
(or)                       ; => nil
```

The C lowerer implements `and`/`or` with nested conditionals. Short-circuiting,
result identity, lexical scope, and tail position are preserved.

### Application and evaluation

An ordinary call has the form `(procedure-expression argument ...)`:

```lisp
((lambda (x) (+ x 1)) 41) ; => 42
```

#### `funcall`

`(funcall procedure-expression argument ...)` explicitly invokes a procedure
with individual evaluated arguments. Zero arguments after the procedure are
allowed.

```lisp
(funcall cons 'x 100)              ; => (x . 100)
(funcall (lambda () (+ 40 2)))     ; => 42
```

#### `apply`

`(apply procedure-expression fixed-argument ... final-list)` supplies zero or
more fixed values followed by the values in a proper list. The final list is
required. The input list spine is not mutated.

```lisp
(apply + '(1 2 3))       ; => 6
(apply + 10 '(20 12))    ; => 42
```

Both `apply` and `funcall` work with native procedures and Scheme procedures,
including variadic prelude procedures. Control forms such as `and` are syntax,
not ordinary callbacks to pass to these forms.

#### `eval`

`(eval expression)` first evaluates its operand to obtain an expression, then
executes that expression in the current lexical environment.

```lisp
(eval '(+ 20 22))                    ; => 42
(let ((x 40)) (eval '(+ x 2)))        ; => 42
```

### Native pair operations

#### `cons`

`(cons first second)` constructs a new pair. If `second` is a proper list, the
result is a proper list; otherwise it is an improper/dotted pair.

```lisp
(cons 'a 'b)      ; => (a . b)
(cons 1 '(2 3))   ; => (1 2 3)
```

#### `car`

`(car pair)` returns its first component. In this dialect, `(car NIL)` is `NIL`.

```lisp
(car '(a . b)) ; => a
(car NIL)      ; => nil
```

#### `cdr`

`(cdr pair)` returns its second component. In this dialect, `(cdr NIL)` is `NIL`.

```lisp
(cdr '(a b c)) ; => (b c)
(cdr '(a . b)) ; => b
(cdr NIL)      ; => nil
```

`car` and `cdr` require a pair or `NIL`; other atoms are errors. Pair mutation
procedures such as `set-car!` are not exposed by the language.

### Native arithmetic and comparisons

Arithmetic is a left-to-right reduction. Integer-only operations produce
integers; the running value promotes to `double` when a float is encountered.
Earlier integer division is already truncated, so introduce a float before
the first division when a fractional result is needed.

#### `+` / `add`

Accepts zero or more numbers; the empty sum is integer zero.

```lisp
(+ 1 2 3)   ; => 6
(add 4 5)   ; => 9
(+)         ; => 0
(+ 1 2.5)   ; => 3.500000
```

#### `-` / `sub`

Requires at least one number. One argument is negated; additional arguments
are subtracted from the first.

```lisp
(- 10 2 3)  ; => 5
(sub 5)     ; => -5
```

#### `*` / `mul`

Accepts zero or more numbers; the empty product is integer one.

```lisp
(* 2 3 4)   ; => 24
(mul 7 6)   ; => 42
(*)         ; => 1
```

#### `/` / `div`

Requires at least one number. Integer division truncates toward zero at each
step. A **single argument is returned unchanged**, not reciprocated.

```lisp
(/ 9 2)       ; => 4
(div -9 2)    ; => -4
(/ 5 2.0)     ; => 2.500000
(/ 5 2 1.0)   ; => 2.000000
(/ 42)        ; => 42
```

Nonnumeric operands, zero divisors, integer intermediate overflow, and
non-finite floating-point arithmetic results raise errors.

#### `<` / `lt`

Requires one or more numbers; returns `T` if each adjacent pair is strictly
increasing. A single number returns `T`.

```lisp
(< 1 2 3)   ; => t
(lt 1 1)    ; => nil
```

#### `>` / `gt`

Requires one or more numbers; returns `T` if each adjacent pair is strictly
decreasing. A single number returns `T`.

```lisp
(> 3 2 1)   ; => t
(gt 2 3)    ; => nil
```

Mixed integer/float ordering comparisons use `double` conversion and therefore
have floating-point precision limits for large integers.

#### `=`

Numeric equality for one or more numbers. It compares adjacent values,
supports mixed integer/float operands, and preserves exact integer distinctions
instead of first rounding every integer to a float. One number returns `T`.

```lisp
(= 2 2.0 2)                          ; => t
(= 2 3)                              ; => nil
(= 9007199254740993 9007199254740992.0) ; => nil
```

### Native equality and type predicates

#### `eq`

`(eq left right)` is type-sensitive. Integers compare by numeric value, symbols
by interned identity, strings by text contents, and pairs/procedures by object
identity. Different types compare false. Two floating-point operands are not
supported by `eq`; use numeric `=` for numbers and prelude `equal?` for structure.

```lisp
(eq 'a 'a)                   ; => t
(eq 2 2.0)                   ; => nil
(eq '(a) '(a))               ; => nil
(let ((xs '(a))) (eq xs xs))  ; => t
(eq + add)                   ; => t
```

Each native type predicate takes exactly one evaluated argument:

| Procedure | Meaning | Concrete example | Result |
| --- | --- | --- | --- |
| `pair?` | A cons cell; not `NIL` | `(pair? '(a . b))` | `t` |
| `number?` | An integer or float | `(number? 3.5)` | `t` |
| `integer?` | Integer storage type, not an integral float | `(integer? 3.0)` | `nil` |
| `symbol?` | A symbol value | `(symbol? 'hello)` | `t` |
| `procedure?` | A native built-in object or a Scheme lambda | `(procedure? (lambda (x) x))` | `t` |

`procedure?` is a representation-level check: native special-form descriptors
also have the built-in type, but still require their special evaluation rules.

### Native output, strings, and errors

#### `print`

`(print value)` writes the formatted value followed by a newline and returns `T`.

```lisp
(print 'hello) ; prints hello, returns t
```

#### `string`

`(string value)` returns a string containing the value's printed representation.
This is formatting, not a character-list constructor. The current printer
displays string contents without quotation marks, even though the result is
a string rather than a number or pair.

```lisp
(string 42)                     ; => 42
(number? (string 42))           ; => nil
(eq (string 42) (string 42))     ; => t
```

#### `error`

`(error description)` takes one evaluated value, prints a user-error diagnostic,
and aborts the current evaluation. Use a quoted symbol or list as a description.

```lisp
(error 'expected-number) ; raises User error: expected-number
```

Completed side effects are retained; there is no rollback or Scheme-level
exception handler. The REPL reports the error and accepts another input line,
without printing a spurious `nil` result. File evaluation stops and the process
returns failure status. Current recovery diagnostics also include VM registers.
Parse errors, arity errors, and arithmetic errors are reported by the core too.

### Tail calls

A call in tail position reuses the existing return continuation. This includes
mutual recursion and calls through `apply` or `funcall`. The final body/`progn`
expression, selected `if` branch, and final expression of a tail-position
`let`, `and`, or `or` preserve tail position.

```lisp
(define (countdown n acc)
  (if (= n 0) acc (countdown (- n 1) (+ acc 1))))
(countdown 30000 0) ; => 30000
```

Control-stack usage remains bounded, but heap data deliberately retained by
the program can still grow. Non-tail recursion retains pending work and uses
control-stack space. Frames captured by closures remain alive through GC.

## Scheme prelude: additional startup procedures

[`lib/prelude.scm`](lib/prelude.scm) is ordinary Scheme source with extensive
implementation comments. The build embeds it using `xxd`, and every new
interpreter loads it before user files or REPL input. No runtime library path
or particular working directory is required. Edit the file and run `make all`
to rebuild the embedded version.

These are ordinary, case-sensitive bindings, not protected native keywords.
They can be called directly, through `apply`/`funcall`, or as callbacks, and
may be redefined with `define` or `set!`. List-traversal procedures validate
proper lists, including rejecting cycles, before invoking callbacks. Callback
arity is checked when the callback is actually invoked.

### Logical and numeric helpers

Except for the explicitly binary comparisons, each takes one argument.
Numeric helpers reject nonnumeric values through the native numeric operations.

| Procedure | Meaning | Concrete example | Result |
| --- | --- | --- | --- |
| `not` | True only when its argument is `NIL` | `(not 0)` | `nil` |
| `null?` | Is this the empty list? | `(null? '())` | `t` |
| `zero?` | Numeric zero, including floating zero | `(zero? 0.0)` | `t` |
| `positive?` | Strictly greater than zero | `(positive? 1.5)` | `t` |
| `negative?` | Strictly less than zero | `(negative? -2)` | `t` |
| `<=` | Binary less-than-or-equal | `(<= 2 2.0)` | `t` |
| `>=` | Binary greater-than-or-equal | `(>= 3 2)` | `t` |
| `abs` | Absolute value | `(abs -7)` | `7` |

`abs` inherits native integer overflow checking: the absolute value of the most
negative integer does not fit in the integer type and raises an error.

### Pair accessors and aliases

Each takes one argument and inherits native `car`/`cdr` behavior. Read composed
accessor letters from right to left: `cadr` means `cdr`, then `car`.
`first` and `rest` initially refer to the same objects as `car` and `cdr`.

| Procedure | Meaning | Concrete example | Result |
| --- | --- | --- | --- |
| `first` | Alias of `car` | `(first '(a b))` | `a` |
| `rest` | Alias of `cdr` | `(rest '(a b))` | `(b)` |
| `caar` | `car` of `car` | `(caar '((a b) c))` | `a` |
| `cadr` | `car` of `cdr` | `(cadr '(a b c))` | `b` |
| `cdar` | `cdr` of `car` | `(cdar '((a b) c))` | `(b)` |
| `cddr` | `cdr` of `cdr` | `(cddr '(a b c))` | `(c)` |
| `caddr` | Third element | `(caddr '(a b c))` | `c` |

### List construction and traversal

#### `list`

`(list value ...)` collects zero or more evaluated values in order. It is a
rest-only Scheme procedure, not evaluator syntax.

```lisp
(list 1 'a NIL) ; => (1 a nil)
(list)         ; => nil
```

#### `append`

`(append list ...)` combines zero or more **proper** lists. It copies prefixes
and shares the final argument's list spine; it does not mutate inputs. One
argument is returned as-is. Unlike some Schemes, an improper final tail is an
error.

```lisp
(append '(1 2) NIL '(3) '(4 5)) ; => (1 2 3 4 5)
(append)                       ; => nil
```

#### `length`

`(length xs)` counts elements in a proper list, not elements of nested lists.

```lisp
(length '(a () (b c))) ; => 3
(length NIL)          ; => 0
```

#### `reverse`

`(reverse xs)` returns a fresh reversed list spine. Elements are shared rather
than deep-copied; the input is not mutated.

```lisp
(reverse '(1 2 3)) ; => (3 2 1)
(reverse NIL)      ; => nil
```

#### `nth`

`(nth index xs)` uses a zero-based **integer** index. Negative and out-of-range
integer indexes return `NIL`. Non-integer indexes, including `1.0`, are errors.
The input must be a proper list even if the index would stop the search early.

```lisp
(nth 1 '(a b c)) ; => b
(nth 9 '(a b))   ; => nil
(nth -1 '(a b))  ; => nil
```

#### `last`

`(last xs)` returns a **fresh one-element list** containing the final element,
not that element alone and not the original tail. This dialect deliberately
preserves `(last NIL)` as a one-element list containing `NIL`.

```lisp
(last '(1 2 3)) ; => (3)
(last NIL)      ; => (nil)
```

#### `butlast`

`(butlast xs)` copies every element except the final one. Empty and singleton
inputs produce `NIL`; inputs are not mutated.

```lisp
(butlast '(1 2 3)) ; => (1 2)
(butlast '(1))     ; => nil
```

#### `list?`

`(list? value)` tests whether the cdr chain ends in `NIL`. It rejects improper
lists, other atoms, and cycles. Elements may themselves be arbitrary values.

```lisp
(list? '(a b))       ; => t
(list? '(a . b))     ; => nil
(list? '((a . b)))   ; => t
```

#### `equal?`

`(equal? left right)` compares acyclic pair structures recursively, including
dotted pairs. Numeric leaves use `=`, so integer and float representations can
compare equal; other leaves use `eq`. It is not a cycle-aware graph comparator,
and deeply nested car structures still consume control-stack space.

```lisp
(equal? '(1 (2 . 3)) '(1.0 (2.0 . 3))) ; => t
(equal? '(a b) '(a c))                 ; => nil
```

#### `map`

`(map f first-list more-list ...)` requires a procedure and at least one proper
list. It invokes `f` once per result, with one element from each input list,
left to right. It stops at the shortest list, after validating all input lists.
Results have a fresh list spine, and input spines are not mutated.

```lisp
(map + '(1 2 3) '(10 20))            ; => (11 22)
(map list '(a b) '(1 2))             ; => ((a 1) (b 2))
(apply map (list + '(1 2) '(10 20)))  ; => (11 22)
```

### Folds

Both folds take `(f initial xs)`, validate `f` and the proper list, and return
`initial` without invoking `f` when the list is empty.

#### `fold-left`

Calls `f` with **accumulator first, element second**, from left to right. Its
walker is tail-recursive.

```lisp
(fold-left + 0 '(1 2 3 4)) ; => 10
(fold-left - 0 '(1 2 3))   ; => -6
(fold-left + 7 NIL)        ; => 7
```

#### `fold-right`

Calls `f` with **element first, folded suffix second**. The rightmost callback
runs first. This implementation is non-tail-recursive and uses linear control
stack space.

```lisp
(fold-right cons NIL '(1 2 3)) ; => (1 2 3)
(fold-right - 0 '(1 2 3))      ; => 2
```

### Selection, search, and effects

These procedures take a unary callback and one proper list. They validate the
whole list before callbacks run, then visit elements left to right. Predicate
results follow the dialect's truth rules; `0` is truthy.

#### `filter`

`(filter predicate xs)` returns a new list containing matching elements in
their original order. Each predicate runs once per element.

```lisp
(filter positive? '(-1 0 2 3 -4 5)) ; => (2 3 5)
```

#### `find`

`(find predicate xs)` returns the first matching **element**, or `NIL` if none
matches. Finding a `NIL` element is indistinguishable from finding nothing;
use `any?` to ask only whether a match exists.

```lisp
(find negative? '(1 -2 -3)) ; => -2
(find negative? '(1 2 3))  ; => nil
```

#### `any?`

`(any? predicate xs)` returns literal `T` on the first match and stops. Empty
lists return `NIL`.

```lisp
(any? zero? '(1 0 3)) ; => t
(any? zero? NIL)      ; => nil
```

#### `every?`

`(every? predicate xs)` returns literal `NIL` on the first failure and stops.
Empty lists return `T`.

```lisp
(every? positive? '(1 2 3)) ; => t
(every? positive? NIL)      ; => t
```

#### `for-each`

`(for-each f xs)` visits every element for its side effects, discards callback
results, and returns `T`, including for an empty list.

```lisp
(define total 0)
(for-each (lambda (x) (set! total (+ total x))) '(1 2 3)) ; => t
total                                                   ; => 6
```

### Membership and association lists

#### `memq`

`(memq value xs)` searches a proper list using `eq`. It returns the **original
suffix** beginning with the first match, or `NIL`. Returning a suffix lets it
represent a successful match whose element is `NIL`. Pair elements compare by
identity, not structure. For numeric searches involving floats, use `find`
with `=` instead of relying on `eq`.

```lisp
(memq 'b '(a b c))  ; => (b c)
(memq NIL '(a () c)) ; => (nil c)
```

#### `assq`

`(assq key entries)` searches a proper association list using `eq` on each
entry's car. It returns the first matching **pair**, not just its value, or
`NIL` when absent. Every visited entry must be a pair; entries after a match
are not inspected.

```lisp
(assq 'name '((name . alice) (age . 42))) ; => (name . alice)
(assq 'missing '((name . alice)))        ; => nil
```

### Higher-order helpers

#### `identity`

`(identity value)` returns that exact value/object without copying it.

```lisp
(identity 42) ; => 42
```

#### `constantly`

`(constantly value)` returns a **unary** closure that ignores its one argument
and returns the captured value. Extra or missing arguments to that returned
closure are errors.

```lisp
((constantly 7) 'ignored)           ; => 7
(map (constantly 7) '(a b c))       ; => (7 7 7)
```

#### `compose`

`(compose f g)` validates two procedures and returns a **unary** closure for
`f(g(value))`. It calls `g` first; the subsequent call to `f` is in tail position.

```lisp
((compose abs (lambda (x) (- x 10))) 3) ; => 7
```

### Internal prelude helpers

These bindings are private by convention, not enforced module boundaries.
They support public procedures' validation and are not intended as extension
points. Each takes one value, returns `T` if valid, and calls `error` otherwise.

| Helper | Concrete successful example | Result |
| --- | --- | --- |
| `_prelude-require-list` | `(_prelude-require-list '(1 2))` | `t` |
| `_prelude-require-procedure` | `(_prelude-require-procedure +)` | `t` |

## Implementation guide

- [`src/flex.l`](src/flex.l) and [`src/bison.y`](src/bison.y): reader and AST
  construction, numeric input range checks, parameter validation, and named
  function-definition shorthand.
- [`src/lm.c`](src/lm.c), [`include/lm.def`](include/lm.def), and
  [`include/lm_states.h`](include/lm_states.h): the explicit control-stack VM,
  state payloads, typed instruction constructors, and tail-call handling.
  `PUSH_SEQUENCE` accepts instructions in execution order and pushes them in
  reverse onto the LIFO stack.
- [`src/syntax.c`](src/syntax.c): runtime lowering of `let`, `and`, and `or`.
  Quoted data is not rewritten, and `or` uses a private predicate-value marker
  to avoid introducing bindings or evaluating an operand twice.
- [`include/builtins.def`](include/builtins.def) and
  [`src/builtins.c`](src/builtins.c): native registration, arity/type checks,
  and canonical built-in objects. Aliases refer to the same built-in object.
- [`lib/prelude.scm`](lib/prelude.scm) and [`src/prelude.c`](src/prelude.c):
  commented Scheme library and embedded-source bootstrap.
- [`src/types.c`](src/types.c), [`src/lm_env.c`](src/lm_env.c), and
  [`src/lm_gc.c`](src/lm_gc.c): typed value construction, shared lexical frames,
  and tracing collection. Interning and environments share the dictionary
  implementation; interned symbol text retains stable addresses.

For C embedding, `lm_create()` loads the prelude and returns NULL on bootstrap
failure. `lm_evaluate()` returns an `EvaluationResult` with separate `success`
and `value` fields, so failure and successful `NIL` are distinguishable.
`lm_progn()` is the value-only compatibility entry point. Call `lm_destroy()`
to release the interpreter's pools and owned payloads.

## Current boundaries

The documented names above are the implemented reference. There is no macro
system, `let*`/`letrec`, `cond`, continuations, vectors, character type, string
literal syntax, Scheme-level pair mutation, or exception handling yet.
Scheme standard-library names such as `eq?`, `display`, and `begin` are not
aliases here: use the documented `eq`, `print`, and `progn` interfaces.
