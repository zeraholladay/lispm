#ifndef LM_ERR_H
#define LM_ERR_H

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
} ErrorCode;

static const char *error_messages[]
    = { [ERR_INTERNAL] = "Internal error occurred",
        [ERR_NOT_A_FUNCTION] = "No such function",
        [ERR_SYMBOL_NOT_FOUND] = "Could not resolve symbol",
        [ERR_INVALID_ARG] = "Invalid argument type or value",
        [ERR_MISSING_ARG] = "Missing required argument",
        [ERR_ARG_TYPE_MISMATCH] = "Argument type mismatch",
        [ERR_ARG_OUT_OF_RANGE] = "Argument value out of range",
        [ERR_UNEXPECTED_ARG] = "Unexpected argument provided",
        [ERR_INVALID_ARG_LENGTH] = "Invalid argument length",
        [ERR_NULL_ARG] = "Argument cannot be null",
        [ERR_ARG_NOT_ITERABLE] = "Argument is not iterable when expected",
        [ERR_DIVISION_BY_0] = "Division by zero" };

#endif