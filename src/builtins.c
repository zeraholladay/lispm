#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "builtins.h"
#include "fmt.h"
#include "lm.h"
#include "lm_err.h"
#include "prims.h"

typedef Cell *(*BuiltinFunction) (LM *lm, Cell *, Cell *);

typedef struct
{
  const char     *name;
  bool            is_special_form;
  int             arity;
  BuiltinFunction function;
} BuiltinDescriptor;

BuiltinWrapper builtin_cells[BUILTIN_COUNT] = {
#define X(id, special, arity, function)                                       \
  [BUILTIN_##id]                                                              \
      = { .free      = 0,                                                     \
          .gc_mark   = 1,                                                     \
          .next_free = NULL,                                                  \
          .ptr = { .type = CELL_TYPE_BUILTIN, .builtin = BUILTIN_##id } },
#include "builtins.def"
#undef X
};

Cell *
builtin_cons (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;
  return cell_make_pair (lm, CAR (arglist), CAR (CDR (arglist)));
}

Cell *
builtin_car (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;

  if (!cell_is_pair_or_nil (CAR (arglist)))
    return lm_fail_value (lm, ERR_INVALID_ARG, "%s", builtin_name (fn));

  return CAR (CAR (arglist));
}

Cell *
builtin_cdr (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;

  if (!cell_is_pair_or_nil (CAR (arglist)))
    return lm_fail_value (lm, ERR_INVALID_ARG, "%s", builtin_name (fn));

  return CDR (CAR (arglist));
}

Cell *
builtin_print (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;
  (void)fn;

  PRINT (CAR (arglist));
  return T;
}

Cell *
builtin_string (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;
  return cell_make_string_take (lm, format (CAR (arglist)));
}

Cell *
builtin_eq (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;
  (void)fn;

  Cell *a = CAR (arglist);
  Cell *b = CAR (CDR (arglist));

  if (a->type != b->type)
    return NIL;

  switch (a->type)
    {
    case CELL_TYPE_NIL:
      return (a == b) ? T : NIL;

    case CELL_TYPE_INTEGER:
      return (a->integer == b->integer) ? T : NIL;

    case CELL_TYPE_SYMBOL:
      return (a->symbol.str == b->symbol.str) ? T : NIL;

    case CELL_TYPE_STRING:
      return (strcmp (a->string, b->string) == 0) ? T : NIL;

    case CELL_TYPE_CONS:
    case CELL_TYPE_BUILTIN:
    case CELL_TYPE_LAMBDA:
      return (a == b) ? T : NIL;

    default:
      return lm_fail_value (lm, ERR_INTERNAL, "eq: unsupported value type");
    }
}

// These predicates expose runtime type information that Scheme code cannot
// safely discover by probing a value with car/cdr or arithmetic operations.
Cell *
builtin_type_predicate (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;
  Cell *value  = CAR (arglist);
  bool  result = false;
  switch (fn->builtin)
    {
    case BUILTIN_INTEGER_P:
      result = cell_has_type (value, CELL_TYPE_INTEGER);
      break;
    case BUILTIN_NUMBER_P:
      result = cell_has_type (value, CELL_TYPE_INTEGER)
               || cell_has_type (value, CELL_TYPE_FLOAT);
      break;
    case BUILTIN_PAIR_P:
      result = cell_is_pair (value);
      break;
    case BUILTIN_PROCEDURE_P:
      result = cell_has_type (value, CELL_TYPE_BUILTIN)
               || cell_has_type (value, CELL_TYPE_LAMBDA);
      break;
    case BUILTIN_SYMBOL_P:
      result = cell_has_type (value, CELL_TYPE_SYMBOL);
      break;
    default:
      return lm_fail_value (lm, ERR_INTERNAL, "unknown type predicate");
    }
  return result ? T : NIL;
}

Cell *
builtin_error (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;
  // The reader has no string literals yet; symbols and lists can carry a
  // readable error description, e.g. (error 'filter-expected-list).
  char *message = format (CAR (arglist));
  Cell *result  = lm_fail_value (lm, ERR_USER, "%s", message);
  free (message);
  return result;
}

typedef enum
{
  NUM_ADD,
  NUM_SUB,
  NUM_MUL,
  NUM_DIV,
} NumericOp;

typedef struct
{
  bool    is_float;
  Integer integer;
  Float   floating;
} Number;

static bool
number_from_cell (LM *lm, Cell *fn, Cell *cell, Number *number)
{
  if (cell_has_type (cell, CELL_TYPE_INTEGER))
    {
      *number = (Number){ .integer = cell->integer };
      return true;
    }

  if (cell_has_type (cell, CELL_TYPE_FLOAT))
    {
      *number = (Number){ .is_float = true, .floating = cell->floating };
      return true;
    }

  lm_fail_value (lm, ERR_ARG_TYPE_MISMATCH, "%s: not a number",
                 builtin_name (fn));
  return false;
}

static Float
number_as_float (Number number)
{
  return number.is_float ? number.floating : (Float)number.integer;
}

static Cell *
number_to_cell (LM *lm, Number number)
{
  return number.is_float ? cell_make_float (lm, number.floating)
                         : cell_make_integer (lm, number.integer);
}

static bool
number_apply (LM *lm, Cell *fn, Number *total, Number item, NumericOp op)
{
  if (total->is_float || item.is_float)
    {
      Float lhs = number_as_float (*total);
      Float rhs = number_as_float (item);

      if (op == NUM_DIV && rhs == 0.0)
        {
          lm_fail_value (lm, ERR_DIVISION_BY_0, "%s: argument is zero",
                         builtin_name (fn));
          return false;
        }

      total->is_float = true;
      switch (op)
        {
        case NUM_ADD:
          total->floating = lhs + rhs;
          break;
        case NUM_SUB:
          total->floating = lhs - rhs;
          break;
        case NUM_MUL:
          total->floating = lhs * rhs;
          break;
        case NUM_DIV:
          total->floating = lhs / rhs;
          break;
        }

      if (!isfinite (total->floating))
        {
          lm_fail_value (lm, ERR_OVERFLOW, "%s", builtin_name (fn));
          return false;
        }

      return true;
    }

  bool overflow = false;
  switch (op)
    {
    case NUM_ADD:
      overflow = __builtin_add_overflow (total->integer, item.integer,
                                         &total->integer);
      break;
    case NUM_SUB:
      overflow = __builtin_sub_overflow (total->integer, item.integer,
                                         &total->integer);
      break;
    case NUM_MUL:
      overflow = __builtin_mul_overflow (total->integer, item.integer,
                                         &total->integer);
      break;
    case NUM_DIV:
      if (item.integer == 0)
        {
          lm_fail_value (lm, ERR_DIVISION_BY_0, "%s: argument is zero",
                         builtin_name (fn));
          return false;
        }
      overflow = total->integer == LLONG_MIN && item.integer == -1;
      if (!overflow)
        total->integer /= item.integer;
      break;
    }

  if (overflow)
    {
      lm_fail_value (lm, ERR_OVERFLOW, "%s", builtin_name (fn));
      return false;
    }

  return true;
}

static Cell *
builtin_arithmetic (LM *lm, Cell *fn, Cell *arglist, NumericOp op)
{
  Number total = { .integer = (op == NUM_MUL) ? 1 : 0 };

  if ((op == NUM_SUB || op == NUM_DIV) && cell_is_nil (arglist))
    return lm_fail_value (lm, ERR_MISSING_ARG, "%s", builtin_name (fn));

  ConsIter iter = cons_iter (arglist);
  if (op == NUM_SUB || op == NUM_DIV)
    {
      if (!number_from_cell (lm, fn, cons_next (&iter), &total))
        return NIL;

      if (op == NUM_SUB && cell_is_nil (iter.cur))
        {
          if (total.is_float)
            total.floating = -total.floating;
          else if (__builtin_sub_overflow ((Integer)0, total.integer,
                                           &total.integer))
            return lm_fail_value (lm, ERR_OVERFLOW, "%s", builtin_name (fn));
          return number_to_cell (lm, total);
        }
    }

  for (Cell *item; (item = cons_next (&iter));)
    {
      Number number;
      if (!number_from_cell (lm, fn, item, &number)
          || !number_apply (lm, fn, &total, number, op))
        return NIL;
    }

  return number_to_cell (lm, total);
}

static Cell *
builtin_compare (LM *lm, Cell *fn, Cell *arglist,
                 bool (*compare) (Number, Number))
{
  if (cell_is_nil (arglist))
    return lm_fail_value (lm, ERR_MISSING_ARG, "%s", builtin_name (fn));

  ConsIter iter = cons_iter (arglist);
  Number   prev;
  if (!number_from_cell (lm, fn, cons_next (&iter), &prev))
    return NIL;

  for (Cell *item; (item = cons_next (&iter));)
    {
      Number next;
      if (!number_from_cell (lm, fn, item, &next))
        return NIL;

      if (!compare (prev, next))
        return NIL;

      prev = next;
    }

  return T;
}

static bool
number_gt (Number lhs, Number rhs)
{
  return (lhs.is_float || rhs.is_float)
             ? number_as_float (lhs) > number_as_float (rhs)
             : lhs.integer > rhs.integer;
}

static bool
number_lt (Number lhs, Number rhs)
{
  return (lhs.is_float || rhs.is_float)
             ? number_as_float (lhs) < number_as_float (rhs)
             : lhs.integer < rhs.integer;
}

static bool
number_eq (Number lhs, Number rhs)
{
  if (!lhs.is_float && !rhs.is_float)
    return lhs.integer == rhs.integer;
  if (lhs.is_float && rhs.is_float)
    return lhs.floating == rhs.floating;

  Integer integer  = lhs.is_float ? rhs.integer : lhs.integer;
  Float   floating = lhs.is_float ? lhs.floating : rhs.floating;
  // Comparing by converting the integer to double could make adjacent large
  // integers appear equal. Instead, range-check before casting the float,
  // then require both the integer value and its float round-trip to match.
  if (!isfinite (floating) || floating < (Float)LLONG_MIN
      || floating >= -(Float)LLONG_MIN)
    return false;
  Integer converted = (Integer)floating;
  return converted == integer && (Float)converted == floating;
}

Cell *
builtin_num_eq (LM *lm, Cell *fn, Cell *arglist)
{
  return builtin_compare (lm, fn, arglist, number_eq);
}

Cell *
builtin_gt (LM *lm, Cell *fn, Cell *arglist)
{
  return builtin_compare (lm, fn, arglist, number_gt);
}

Cell *
builtin_lt (LM *lm, Cell *fn, Cell *arglist)
{
  return builtin_compare (lm, fn, arglist, number_lt);
}

Cell *
builtin_add (LM *lm, Cell *fn, Cell *arglist)
{
  return builtin_arithmetic (lm, fn, arglist, NUM_ADD);
}

Cell *
builtin_sub (LM *lm, Cell *fn, Cell *arglist)
{
  return builtin_arithmetic (lm, fn, arglist, NUM_SUB);
}

Cell *
builtin_mul (LM *lm, Cell *fn, Cell *arglist)
{
  return builtin_arithmetic (lm, fn, arglist, NUM_MUL);
}

Cell *
builtin_div (LM *lm, Cell *fn, Cell *arglist)
{
  return builtin_arithmetic (lm, fn, arglist, NUM_DIV);
}

static const BuiltinDescriptor builtin_table[BUILTIN_COUNT] = {
#define X(id, special, arity, function) { #id, special, arity, function },
#include "builtins.def"
#undef X
};

Cell *
builtin_value (BuiltinId id)
{
  return (unsigned)id < BUILTIN_COUNT ? &builtin_cells[id].ptr : NULL;
}

const char *
builtin_name (const Cell *value)
{
  return cell_has_type (value, CELL_TYPE_BUILTIN)
                 && (unsigned)value->builtin < BUILTIN_COUNT
             ? builtin_table[value->builtin].name
             : "invalid built-in";
}

bool
builtin_is_special_form (const Cell *value)
{
  return cell_has_type (value, CELL_TYPE_BUILTIN)
         && (unsigned)value->builtin < BUILTIN_COUNT
         && builtin_table[value->builtin].is_special_form;
}

Cell *
dispatch_builtin (LM *lm, Cell *function, Cell *arguments)
{
  if (!cell_is_proper_list (arguments))
    return lm_fail_value (lm, ERR_INVALID_ARG,
                          "built-in: improper argument list");
  if (!cell_has_type (function, CELL_TYPE_BUILTIN)
      || (unsigned)function->builtin >= BUILTIN_COUNT)
    return lm_fail_value (lm, ERR_INTERNAL, "invalid built-in");

  BuiltinDescriptor descriptor = builtin_table[function->builtin];
  if (!descriptor.function)
    return lm_fail_value (lm, ERR_INTERNAL, "%s: not a native procedure",
                          descriptor.name);

  size_t received = length (arguments);
  if (descriptor.arity >= 0 && (size_t)descriptor.arity != received)
    {
      ErrorCode err = (received < (size_t)descriptor.arity)
                          ? ERR_MISSING_ARG
                          : ERR_UNEXPECTED_ARG;
      return lm_fail_value (lm, err, "%s", descriptor.name);
    }

  return descriptor.function (lm, function, arguments);
}
