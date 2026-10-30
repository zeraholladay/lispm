#include "prelude.h"
#include "parser.h"
#include "prims.h"

// lib/prelude.scm is the source of truth. The build embeds its bytes so
// neither the executable nor C API users depend on a
// working-directory-relative path.
static const char prelude_source[] = {
#include "prelude.inc"
};

bool
prelude_load (LM *lm)
{
  Cell *forms = NULL;
  if (!parser_buf (prelude_source, &forms, lm))
    return false;

  // The prelude ends with T as a success marker. Parse/evaluation failures
  // must fail construction rather than expose a partly bootstrapped VM.
  EvaluationResult result = lm_evaluate (lm, forms);
  return result.success && result.value == T;
}
