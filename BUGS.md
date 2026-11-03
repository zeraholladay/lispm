# Bug Tracking

Findings from a code review of the interpreter. Behavioral probes used an
AddressSanitizer + UBSan build; defensive concerns are identified separately.
ASan reported no memory errors on any probe, and tail calls stayed bounded
(20,000 self-calls and 5,000 mutual calls through `let` both completed).

Resolved entries retain the original symptoms for reference. Source line
references describe the reviewed revision and may shift as fixes are applied.

## Bugs

### 1. [Resolved] Calling a closure via `eval` returns the closure instead of calling it

**Resolution:** Only an uncaptured lambda in an empty parser wrapper takes the
capture shortcut. Existing closures go through normal call handling, preserving
their captured environment. Covered by `test_eval_calls_closure_value` in
`test/test_eval.c`.

`src/lm.c:187`

```scheme
(define f (lambda (x) (* x 10)))
(eval (list f 5))        ; => #<LAMBDA (x) ((* x 10))>, expected 50
```

Previously, any call form whose first element was a lambda cell was handled by
`lambda_capture`, which dropped the arguments. It also re-captured the function
in the *current* environment, so a closure passed this way silently switched to
dynamic scope. This shortcut is only meant for the parser's `(<lambda-ast>)`
wrapper. It now requires `cell_is_nil(car->lambda.env)` and an empty `cdr`,
and lets everything else go through `eval_call`.

### 2. [Resolved] `eq` raises an internal error on floats

**Resolution:** `eq` compares two floats by numeric value. Covered by `test_eq`
in `test/test_eval_bool.c`, including a float lookup through `memq`.

`src/builtins.c:93`

There was no `CELL_TYPE_FLOAT` case, so `(eq 1.0 1.0)` and
`(memq 1.5 '(1.5))` failed with "Internal error: eq: unsupported value type".
`assq` with a float key failed the same way.

### 3. [Resolved] `(eq 't t)` returns nil

**Resolution:** Symbol equality falls back to length and text comparison when
string pointers differ. Covered by `test_eq` in `test/test_eval_bool.c`.

`src/prims.c:5-24` vs `src/builtins.c:102`

`eq` previously compared symbols only by string pointer. The reader interns
`"t"`, but the static `T` (and `QUOTE`) use plain string literals, so `'t` and
`t` did not compare equal.

### 4. [Resolved] The lexer doesn't require a delimiter after a number

**Resolution:** Malformed numeric tokens are rejected as a whole with a located
diagnostic. Covered by `test_numeric_errors_have_token_spans` in
`test/test_parser.c`, including valid delimiter-separated input.

`src/flex.l:28-30`

```scheme
(+ 1-2)      ; => -1   (lexed as 1 and -2)
1e3          ; => 1, then "Could not resolve symbol: e3"
'(1.5.5)     ; => (1.500000 . 5)
3abc         ; => 3 and abc
```

All of these are now lexer errors; the comments above show the old behavior.

### 5. Reserved words can't be used as data, and `set` is unusable

`src/flex.l:91-107`, `src/bison.y:180`

- `if`, `define`, `let` and `lambda` are grammar tokens, not symbols. So
  `'(if a)` and `(procedure? if)` are syntax errors.
- The `SET` token matches `set`, not `set!`. As a result, `(set x 1)` parses
  and then fails at runtime with "Could not resolve symbol: set". Writing
  `(define set 1)` is a syntax error. The real form, `set!`, only works because
  it falls through to the ordinary symbol path.

### 6. [Resolved] Fixed-interval GC caused excessive heap rescanning

**Resolution:** Replaced the fixed VM-step interval with an allocation budget.
After collection, the budget is the maximum of live cells, half the reserved
pool capacity, and `LM_GC_MIN_ALLOCATIONS` (4096 by default). Pressure persists
across evaluations and is serviced at VM safe points, never inside allocation
or parsing. Per-pool occupied-slot counts measure survivors without another
full heap scan. Covered by the `GC Scheduling` suite in `test/test_gc.c`.

`src/lm.c:575`

Previously, a full mark and sweep of every pool ran every 256 VM steps, however
large the live heap was. Original review timings for summing an n-element list
(release build):

| n    | time   |
| ---- | ------ |
| 2000 | 0.09 s |
| 4000 | 0.28 s |
| 8000 | 0.90 s |

Building a 20k-element list and `map`ping over it didn't finish in 5 minutes
under ASan in the original review. The new scheduler amortizes full collections
over allocations proportional to heap size; individual collections still scan
the retained pools and pause execution.

### 7. `(/ x)` returns `x`

`src/builtins.c:297-311`

One-argument `-` negates, but one-argument `/` returns its argument unchanged
instead of the reciprocal.

## Lower severity

- **[Resolved] `let` checked names less strictly than `lambda`**
  (`src/syntax.c`): Duplicate and reserved binding names are now rejected by
  the lowerer before any initializer runs. Unlike lambda formals, these checks
  happen at evaluation time. Covered by
  `test_invalid_let_bindings_fail_before_initializers` in `test/test_syntax.c`,
  asserting explicit evaluation failure and no initializer side effects.
- **[Resolved] Fragile GC mark loop** (`src/lm_gc.c`): A NULL child previously
  could be mistaken for an empty work stack, leaving pending reachable cells
  unmarked. The collector now filters NULL roots and children before pushing
  them. This is defensive hardening; ordinary Scheme values did not provide a
  reproducer. Covered by `test_gc_null_child_does_not_hide_pending_children`
  in `test/test_eval.c`, using a C-created graph in the final root traversal.
- **Returned values aren't GC roots** (`src/lm.c:589`): an embedder that holds
  the result of `lm_evaluate` across another evaluation can have it collected.
  The REPL and the prelude loader use the value immediately, so they're safe.
- **Float printing loses information** (`src/fmt.c:106`): `%f` prints
  `0.0000001` as `0.000000`.
- **`'nil` is a truthy symbol, printed as `nil`.** The prelude documents this,
  but `(car '(nil))` prints `nil` while being true, which is confusing.
- **[Resolved] Dead code and a stale comment** in `src/lm.c`: Removed the
  always-true `if (else_form)` branch and updated the lambda-wrapper comment.
  A missing alternative still evaluates to NIL.
- **[Resolved] `'` was offered as a tab-completion candidate**
  (`src/keywords.g`): Removed the reader abbreviation from the keyword table;
  quote syntax still works through the lexer/parser. Covered by
  `test_completion_keywords_and_operators` in `test/test_readline.c`.
