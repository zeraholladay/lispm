#ifndef BUILTINS_H
#define BUILTINS_H

#include "palloc.h"
#include "types.h"

typedef struct builtin_wrapper BuiltinWrapper;
struct builtin_wrapper
{
  PALLOC_WRAPPER_FIELDS (BuiltinWrapper)
  Cell ptr;
};

// Exactly one definition, in builtins.c. Generated keyword tables need these
// addresses as constant initializers; other callers should use builtin_value.
// Static built-ins stay permanently marked and are never pool allocations.
extern BuiltinWrapper builtin_cells[BUILTIN_COUNT];

Cell       *builtin_value (BuiltinId id);
const char *builtin_name (const Cell *value);
bool        builtin_is_special_form (const Cell *value);
Cell       *dispatch_builtin (LM *lm, Cell *function, Cell *arguments);

#endif
