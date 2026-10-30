#ifndef LM_ERR_H
#define LM_ERR_H

#include <stdbool.h>

#include "lm.h"
#include "types.h"

typedef enum
{
  ERR_NOT_A_FN,          // Function DNE
  ERR_INTERNAL,          // Internal error (bug)
  ERR_SYM_NOT_FOUND,     // Symbol undefined
  ERR_INVALID_ARG,       // Invalid type or value for argument
  ERR_MISSING_ARG,       // Missing required argument
  ERR_ARG_TYPE_MISMATCH, // Argument has wrong type
  ERR_ARG_OUT_OF_RANGE,  // Value out of range
  ERR_UNEXPECTED_ARG,    // Extra or unexpected argument
  ERR_INVALID_ARG_LEN,   // Length of argument is invalid
  ERR_NULL_ARG,          // Null argument where not allowed
  ERR_ARG_NOT_ITER,      // Argument expected to be iterable
  ERR_DIVISION_BY_0,     // Division by 0
  ERR_OVERFLOW,          // Overflow (e.g., push to full stack)
  ERR_UNDERFLOW,         // Underflow (e.g., pop from empty stack)
  ERR_USER,              // Explicitly raised by the Scheme error procedure
} ErrorCode;

// Raise a VM error and return a failure value appropriate to the caller.
// A returned NIL is an error only when has_error is set; NIL is also a valid
// successful Lisp value. The evaluator performs recovery at its boundary.
bool lm_fail (LM *lm, ErrorCode code, const char *fmt, ...)
    __attribute__ ((format (printf, 3, 4)));
Cell *lm_fail_value (LM *lm, ErrorCode code, const char *fmt, ...)
    __attribute__ ((format (printf, 3, 4)));
void *lm_fail_pointer (LM *lm, ErrorCode code, const char *fmt, ...)
    __attribute__ ((format (printf, 3, 4)));

#endif // LM_ERR_H
