# Feature ideas

Potential next features for the interpreter. The order below is a suggested
starting point, not a commitment. Completed foundations include parse-time
source diagnostics, matching `[]` list delimiters, and live-environment REPL
completion.

## REPL and discovery

- [ ] **Multiline input.** Continue reading incomplete expressions with a
  continuation prompt. Distinguish incomplete input from invalid syntax, and
  retain accurate source excerpts across submitted lines.
- [ ] **Environment inspection.** Add an `apropos`-style name search and a way
  to list available bindings.
- [ ] **Help and parameter hints.** Associate documentation and argument
  information with built-ins, prelude functions, and user-defined procedures;
  expose it through help commands and eventually interactive hints.
- [ ] **Context-aware completion.** Prefer callable names in operator position
  and infer local names inside unfinished `let` and `lambda` forms.

## Language additions

- [ ] **`cond`.** Lower multi-branch conditionals into existing core forms.
  Support an optional final `else` clause, evaluate tests exactly once, and
  preserve short-circuiting and tail position.
- [ ] **`let*`.** Add sequential bindings so later initializers can refer to
  earlier bindings; lower into nested `let` expressions.
- [ ] **Named `let`.** Provide convenient local recursive loops while preserving
  proper tail calls.
- [ ] **`letrec`.** Support mutually recursive local procedures. Define how
  uninitialized bindings behave and diagnose invalid early references.
- [ ] **String literals and operations.** Read quoted strings with documented
  escape sequences. Add length, concatenation, and content comparison, reusing
  existing string values.
- [ ] **Structural equality.** Add recursive comparison for lists and strings,
  with clear behavior for numbers and procedure values. Choose a name
  consistent with the existing equality operations.
- [ ] **Quasiquote.** Support quasiquote, unquote, and unquote-splicing, including
  nested quasiquotes, list splicing, and useful errors for invalid positions.

## Continuations and control

- [ ] **`call-with-current-continuation` / `call/cc`.** Add full, reusable
  continuations as callable values.
  - [ ] Specify invocation arity and behavior across evaluation boundaries,
    including capture in one REPL submission and invocation in a later one.
  - [ ] Capture the control stack, value stack, and current environment in an
    owned snapshot; invoking a continuation replaces the current execution
    state and supplies the result of the captured `call/cc` expression.
  - [ ] Trace retained values and environments through GC and release snapshot
    storage when the continuation becomes unreachable.
  - [ ] Preserve shared mutable bindings: restoring control does not roll back
    mutations.
  - [ ] Test early exits, nested captures, invocation after the original call
    returns, repeated invocation, tail calls, and garbage collection.
- [ ] **`dynamic-wind`.** Run entry and exit procedures when entering or leaving
  a dynamic extent, including jumps caused by continuation invocation.
- [ ] **Recoverable errors.** Let Scheme code handle failures and perform
  cleanup. Define how handlers interact with continuations and dynamic extents.

## Files and larger programs

- [ ] **`load`.** Evaluate a source file from Scheme, preserving filename
  diagnostics and specifying path resolution and failure behavior.
- [ ] **Ports and file I/O.** Introduce input/output ports and operations for
  reading and writing data, with explicit resource ownership and closing.
- [ ] **Modules.** Support explicit imports and exports so programs can be split
  into files without relying on a shared global namespace.
- [ ] **Macros.** Choose an expansion model, including whether macros are
  hygienic, how syntax is represented, and how expansion errors retain source
  locations.

## Runtime diagnostics

- [ ] **Runtime source locations.** Retain source metadata beyond parsing so
  evaluation errors can identify their originating expression.
- [ ] **Scheme stack traces.** Show useful procedure names and source locations
  on failure, with a documented treatment of tail calls.

## Implementation approach

- Prefer derived syntax lowered into existing core forms and library functions
  written in the Scheme prelude where practical.
- Specify evaluation order, tail-position behavior, and errors for each new
  language feature.
- Add focused regression coverage and update the language reference as features
  become available.
