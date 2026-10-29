#include "syntax.h"
#include "keywords.h"
#include "prims.h"

static PrimWrapper predicate_marker = {
  .free      = 0,
  .gc_mark   = 1,
  .next_free = NULL,
  .ptr       = { .type = TYPE_UNKNOWN },
};

Cell *const syntax_predicate_value = &predicate_marker.ptr;

static Cell *
core_if (LM *lm, Cell *test, Cell *consequent, Cell *alternative)
{
  // Embed the core procedure object, not a name that generated code must look
  // up. Lowering never depends on user bindings of prelude helpers.
  Cell *fn = keyword_lookup ("if", 2);
  return CONS (fn, CONS (test, LIST2 (consequent, alternative, lm), lm), lm);
}

Cell *
syntax_lower (LM *lm, ThunkEnum form, Cell *arguments)
{
  if (!proper_list_p (arguments))
    return lm_err_null (lm, ERR_INVALID_ARG, "syntax: improper arguments");

  if (form == THUNK_LET)
    {
      if (length (arguments) < 2)
        return lm_err_null (lm, ERR_MISSING_ARG, "let");
      Cell *bindings = CAR (arguments);
      if (!proper_list_p (bindings))
        return lm_err_null (lm, ERR_INVALID_ARG, "let: improper binding list");
      Cell *names  = NIL;
      Cell *values = NIL;
      for (Cell *p = bindings; CONSP (p); p = CDR (p))
        {
          Cell *binding = CAR (p);
          if (!proper_list_p (binding) || length (binding) != 2
              || !IS_INST (CAR (binding), SYMBOL))
            return lm_err_null (lm, ERR_INVALID_ARG,
                                "let: expected (name value)");
          names  = CONS (CAR (binding), names, lm);
          values = CONS (CADR (binding), values, lm);
        }

      // (let ((x a) (y b)) body ...) -> ((lambda (x y) body ...) a b).
      // The operator captures the current environment before initializers
      // run. All initializers then run left-to-right in that outer
      // environment.
      Cell *lambda = LAMBDA (reverse_inplace (names), CDR (arguments), lm);
      return CONS (LIST1 (lambda, lm), reverse_inplace (values), lm);
    }

  if (form != THUNK_AND && form != THUNK_OR)
    return lm_err_null (lm, ERR_INTERNAL, "unknown derived syntax");
  if (NILP (arguments))
    return form == THUNK_AND ? T : NIL;

  // Build nested conditionals from right to left, without recursive C calls
  // or mutation of the source AST. The final operand stays in tail position.
  Cell *reversed = NIL;
  for (Cell *p = arguments; CONSP (p); p = CDR (p))
    reversed = CONS (CAR (p), reversed, lm);
  Cell *result = CAR (reversed);
  for (Cell *p = CDR (reversed); CONSP (p); p = CDR (p))
    result = form == THUNK_AND
                 ? core_if (lm, CAR (p), result, NIL)
                 : core_if (lm, CAR (p), syntax_predicate_value, result);

  // or's placeholder returns the exact tested object, once. A temporary
  // lambda/let binding would add scope around later operands and could change
  // where define writes. No introduced binding also means no name capture.
  return result;
}
