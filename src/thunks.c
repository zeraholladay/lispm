#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fmt.h"
#include "lm.h"
#include "prims.h"
#include "thunks.h"

typedef Cell *(*ThunkFn) (LM *lm, Cell *, Cell *);

typedef struct
{
  const char *name;
  bool        sform;
  int         arity;
  ThunkFn     fn;
} Thunk;

// thunks

Cell *
thunk_append (LM *lm, Cell *fn, Cell *arglist)
{
  for (Cell *p = arglist; CONSP (p); p = CDR (p))
    if (!proper_list_p (CAR (p)))
      return lm_err_nil (lm, ERR_INVALID_ARG, "%s: improper list",
                         thunk_get_name (fn));

  Cell *result = CAR (arglist);

  for (Cell *cdr = CDR (arglist); !NILP (cdr); cdr = CDR (cdr))
    result = append_list (lm, result, CAR (cdr));

  return result;
}

Cell *
thunk_cons (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;
  return CONS (CAR (arglist), CAR (CDR (arglist)), lm);
}

Cell *
thunk_car (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;

  if (!LISTP (CAR (arglist)))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return CAR (CAR (arglist));
}

Cell *
thunk_cdr (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;

  if (!LISTP (CAR (arglist)))
    return lm_err_nil (lm, ERR_INVALID_ARG, thunk_get_name (fn));

  return CDR (CAR (arglist));
}

Cell *
thunk_print (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;
  (void)fn;

  PRINT (CAR (arglist));
  return T;
}

Cell *
thunk_string (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;
  return string_take (lm, format (CAR (arglist)));
}

Cell *
thunk_eq (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;
  (void)fn;

  Cell *a = CAR (arglist);
  Cell *b = CAR (CDR (arglist));

  if (a->type != b->type)
    return NIL;

  switch (a->type)
    {
    case TYPE_NIL:
      return (a == b) ? T : NIL;

    case TYPE_INTEGER:
      return (a->integer == b->integer) ? T : NIL;

    case TYPE_SYMBOL:
      return (a->symbol.str == b->symbol.str) ? T : NIL;

    case TYPE_STRING:
      return (strcmp (a->string, b->string) == 0) ? T : NIL;

    case TYPE_CONS:
    case TYPE_THUNK:
    case TYPE_LAMBDA:
      return (a == b) ? T : NIL;

    default:
      return lm_err_nil (lm, ERR_INTERNAL, "null thunk");
      ;
    }
}

// These predicates expose runtime type information that Scheme code cannot
// safely discover by probing a value with car/cdr or arithmetic operations.
Cell *
thunk_type_predicate (LM *lm, Cell *fn, Cell *arglist)
{
  (void)lm;
  Cell *value  = CAR (arglist);
  bool  result = false;
  switch (fn->thunk)
    {
    case THUNK_INTEGER_P:
      result = IS_INST (value, INTEGER);
      break;
    case THUNK_NUMBER_P:
      result = IS_INST (value, INTEGER) || IS_INST (value, FLOAT);
      break;
    case THUNK_PAIR_P:
      result = CONSP (value);
      break;
    case THUNK_PROCEDURE_P:
      result = IS_INST (value, THUNK) || IS_INST (value, LAMBDA);
      break;
    case THUNK_SYMBOL_P:
      result = IS_INST (value, SYMBOL);
      break;
    default:
      return lm_err_nil (lm, ERR_INTERNAL, "unknown type predicate");
    }
  return result ? T : NIL;
}

Cell *
thunk_error (LM *lm, Cell *fn, Cell *arglist)
{
  (void)fn;
  // The reader has no string literals yet; symbols and lists can carry a
  // readable error description, e.g. (error 'filter-expected-list).
  char *message = format (CAR (arglist));
  Cell *result  = lm_err_nil (lm, ERR_USER, "%s", message);
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
  Float   _float;
} Number;

static bool
number_from_cell (LM *lm, Cell *fn, Cell *cell, Number *number)
{
  if (IS_INST (cell, INTEGER))
    {
      *number = (Number){ .integer = cell->integer };
      return true;
    }

  if (IS_INST (cell, FLOAT))
    {
      *number = (Number){ .is_float = true, ._float = cell->_float };
      return true;
    }

  lm_err_nil (lm, ERR_ARG_TYPE_MISMATCH, "%s: not a number",
              thunk_get_name (fn));
  return false;
}

static Float
number_as_float (Number number)
{
  return number.is_float ? number._float : (Float)number.integer;
}

static Cell *
number_to_cell (LM *lm, Number number)
{
  return number.is_float ? FLOAT (number._float, lm)
                         : INTEGER (number.integer, lm);
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
          lm_err_nil (lm, ERR_DIVISION_BY_0, "%s: argument is zero",
                      thunk_get_name (fn));
          return false;
        }

      total->is_float = true;
      switch (op)
        {
        case NUM_ADD:
          total->_float = lhs + rhs;
          break;
        case NUM_SUB:
          total->_float = lhs - rhs;
          break;
        case NUM_MUL:
          total->_float = lhs * rhs;
          break;
        case NUM_DIV:
          total->_float = lhs / rhs;
          break;
        }

      if (!isfinite (total->_float))
        {
          lm_err_nil (lm, ERR_OVERFLOW, "%s", thunk_get_name (fn));
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
          lm_err_nil (lm, ERR_DIVISION_BY_0, "%s: argument is zero",
                      thunk_get_name (fn));
          return false;
        }
      overflow = total->integer == LLONG_MIN && item.integer == -1;
      if (!overflow)
        total->integer /= item.integer;
      break;
    }

  if (overflow)
    {
      lm_err_nil (lm, ERR_OVERFLOW, "%s", thunk_get_name (fn));
      return false;
    }

  return true;
}

static Cell *
thunk_arithmetic (LM *lm, Cell *fn, Cell *arglist, NumericOp op)
{
  Number total = { .integer = (op == NUM_MUL) ? 1 : 0 };

  if ((op == NUM_SUB || op == NUM_DIV) && NILP (arglist))
    return lm_err_nil (lm, ERR_MISSING_ARG, "%s", thunk_get_name (fn));

  ConsIter iter = cons_iter (arglist);
  if (op == NUM_SUB || op == NUM_DIV)
    {
      if (!number_from_cell (lm, fn, cons_next (&iter), &total))
        return NIL;

      if (op == NUM_SUB && NILP (iter.cur))
        {
          if (total.is_float)
            total._float = -total._float;
          else if (__builtin_sub_overflow ((Integer)0, total.integer,
                                           &total.integer))
            return lm_err_nil (lm, ERR_OVERFLOW, "%s", thunk_get_name (fn));
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
thunk_compare (LM *lm, Cell *fn, Cell *arglist,
               bool (*compare) (Number, Number))
{
  if (NILP (arglist))
    return lm_err_nil (lm, ERR_MISSING_ARG, "%s", thunk_get_name (fn));

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
    return lhs._float == rhs._float;

  Integer integer  = lhs.is_float ? rhs.integer : lhs.integer;
  Float   floating = lhs.is_float ? lhs._float : rhs._float;
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
thunk_num_eq (LM *lm, Cell *fn, Cell *arglist)
{
  return thunk_compare (lm, fn, arglist, number_eq);
}

Cell *
thunk_gt (LM *lm, Cell *fn, Cell *arglist)
{
  return thunk_compare (lm, fn, arglist, number_gt);
}

Cell *
thunk_lt (LM *lm, Cell *fn, Cell *arglist)
{
  return thunk_compare (lm, fn, arglist, number_lt);
}

Cell *
thunk_add (LM *lm, Cell *fn, Cell *arglist)
{
  return thunk_arithmetic (lm, fn, arglist, NUM_ADD);
}

Cell *
thunk_sub (LM *lm, Cell *fn, Cell *arglist)
{
  return thunk_arithmetic (lm, fn, arglist, NUM_SUB);
}

Cell *
thunk_mul (LM *lm, Cell *fn, Cell *arglist)
{
  return thunk_arithmetic (lm, fn, arglist, NUM_MUL);
}

Cell *
thunk_div (LM *lm, Cell *fn, Cell *arglist)
{
  return thunk_arithmetic (lm, fn, arglist, NUM_DIV);
}

static const Thunk thunk_table[_THUNK_END] = {
#define X(sym, is_l, ar, fn) { #sym, is_l, ar, fn },
#include "thunks.def"
#undef X
};

const char *
thunk_get_name (Cell *c)
{
  return (IS_INST (c, THUNK)) ? thunk_table[c->thunk].name : "bad thunk!";
}

bool
thunk_sf_bool (Cell *c)
{
  return thunk_table[c->thunk].sform;
}

Cell *
thunker (LM *lm, Cell *fn, Cell *arglist)
{
  if (!proper_list_p (arglist))
    return lm_err_nil (lm, ERR_INVALID_ARG, "thunker: improper argument list");
  if (!fn)
    return lm_err_nil (lm, ERR_INTERNAL, "null thunk");

  Thunk thunk = thunk_table[fn->thunk];

  if (!thunk.fn)
    return lm_err_nil (lm, ERR_INTERNAL, thunk.name ?: "not a thunk");

  int received = (int)length (arglist);

  if (thunk.arity > 0 && thunk.arity != received)
    {
      Err err
          = (received < thunk.arity) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
      return lm_err_nil (lm, err, "%s", thunk.name);
    }

  return thunk.fn (lm, fn, arglist);
}
