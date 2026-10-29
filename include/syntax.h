#ifndef SYNTAX_H
#define SYNTAX_H

#include "types.h"

// Internal AST placeholder: a true branch returns the predicate already held
// by the core if continuation. It cannot be written or bound by Scheme code.
extern Cell *const syntax_predicate_value;

// Lower one derived form when it is evaluated. Quoted data is never traversed
// or rewritten, and eval-created expressions use this same expansion path.
Cell *syntax_lower (LM *lm, ThunkEnum form, Cell *arguments);

#endif
