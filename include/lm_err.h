#ifndef LM_ERR_H
#define LM_ERR_H

#include <stdbool.h>

#include "lm.h"
#include "types.h"

typedef enum
{
  ERR_NOT_A_FUNCTION,     // Function DNE
  ERR_INTERNAL,           // Internal error (bug)
  ERR_SYMBOL_NOT_FOUND,   // Symbol undefined
  ERR_INVALID_ARG,        // Invalid type or value for argument
  ERR_MISSING_ARG,        // Missing required argument
  ERR_ARG_TYPE_MISMATCH,  // Argument has wrong type
  ERR_ARG_OUT_OF_RANGE,   // Value out of range
  ERR_UNEXPECTED_ARG,     // Extra or unexpected argument
  ERR_INVALID_ARG_LENGTH, // Length of argument is invalid
  ERR_NULL_ARG,           // Null argument where not allowed
  ERR_ARG_NOT_ITERABLE,   // Argument expected to be iterable
  ERR_DIVISION_BY_0,      // Division by 0
  ERR_OVERFLOW,           // Overflow (e.g., push to full stack)
  ERR_UNDERFLOW,          // Underflow (e.g., pop from empty stack)
} Err;

bool  lm_err_bool (LM *lm, Err code, const char *fmt, ...);
Cell *lm_err_nil (LM *lm, Err code, const char *fmt, ...);
void *lm_err_null (LM *lm, Err code, const char *fmt, ...);

#endif // LM_ERR_H
