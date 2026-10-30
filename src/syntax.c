#include "syntax.h"
#include "builtins.h"
#include "lm_err.h"
#include "prims.h"

static PrimWrapper predicate_marker = {
  .free      = 0,
  .gc_mark   = 1,
  .next_free = NULL,
  .ptr       = { .type = CELL_TYPE_UNKNOWN },
};

Cell *const syntax_predicate_value = &predicate_marker.ptr;

static Cell *
core_if (LM *lm, Cell *test, Cell *consequent, Cell *alternative)
{
  // Embed the core procedure object, not a name that generated code must look
  // up. Lowering never depends on user bindings of prelude helpers.
  Cell *fn = builtin_value (BUILTIN_IF);
  return cell_make_pair (
      lm, fn,
      cell_make_pair (lm, test, cell_list2 (lm, consequent, alternative)));
}

Cell *
syntax_lower (LM *lm, BuiltinId form, Cell *arguments)
{
  if (!cell_is_proper_list (arguments))
    return lm_fail_pointer (lm, ERR_INVALID_ARG, "syntax: improper arguments");

  if (form == BUILTIN_LET)
    {
      if (length (arguments) < 2)
        return lm_fail_pointer (lm, ERR_MISSING_ARG, "let");
      Cell *bindings = CAR (arguments);
      if (!cell_is_proper_list (bindings))
        return lm_fail_pointer (lm, ERR_INVALID_ARG,
                                "let: improper binding list");
      Cell *names  = NIL;
      Cell *values = NIL;
      for (Cell *p = bindings; cell_is_pair (p); p = CDR (p))
        {
          Cell *binding = CAR (p);
          if (!cell_is_proper_list (binding) || length (binding) != 2
              || !cell_has_type (CAR (binding), CELL_TYPE_SYMBOL))
            return lm_fail_pointer (lm, ERR_INVALID_ARG,
                                    "let: expected (name value)");
          names  = cell_make_pair (lm, CAR (binding), names);
          values = cell_make_pair (lm, CADR (binding), values);
        }

      // (let ((x a) (y b)) body ...) -> ((lambda (x y) body ...) a b).
      // The operator captures the current environment before initializers
      // run. All initializers then run left-to-right in that outer
      // environment.
      Cell *lambda
          = cell_make_lambda (lm, reverse_inplace (names), CDR (arguments));
      return cell_make_pair (lm, cell_list1 (lm, lambda),
                             reverse_inplace (values));
    }

  if (form != BUILTIN_AND && form != BUILTIN_OR)
    return lm_fail_pointer (lm, ERR_INTERNAL, "unknown derived syntax");
  if (cell_is_nil (arguments))
    return form == BUILTIN_AND ? T : NIL;

  // Build nested conditionals from right to left, without recursive C calls
  // or mutation of the source AST. The final operand stays in tail position.
  Cell *reversed = NIL;
  for (Cell *p = arguments; cell_is_pair (p); p = CDR (p))
    reversed = cell_make_pair (lm, CAR (p), reversed);
  Cell *result = CAR (reversed);
  for (Cell *p = CDR (reversed); cell_is_pair (p); p = CDR (p))
    result = form == BUILTIN_AND
                 ? core_if (lm, CAR (p), result, NIL)
                 : core_if (lm, CAR (p), syntax_predicate_value, result);

  // or's placeholder returns the exact tested object, once. A temporary
  // lambda/let binding would add scope around later operands and could change
  // where define writes. No introduced binding also means no name capture.
  return result;
}
