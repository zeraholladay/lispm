#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "list.h"
#include "safe_str.h"
#include "types.h"

#define CINTEGER_TYPE_FMT "%lld"

#define LOG10_2 0.30103

#define INTEGER_TYPE_STR_MAX_SIZE                                             \
  ((size_t)(sizeof (Integer) * CHAR_BIT * LOG10_2 + 3))

// helper
size_t
list_append_strdup (List *list, char *str)
{
  if (!str)
    {
      return 0;
    }

  size_t len = strlen (str);

  char *dup = safe_strndup (str, len);
  if (!dup)
    {
      return 0;
    }

  if (list_append (list, dup) < 0)
    {
      return 0;
    }

  return len;
}

// Type eq
static inline int
type_eq (Node *self, Node *other)
{
  return type (self) == type (other);
}

// NIL type
static int
nil_eq (Node *self, Node *other)
{
  (void)self;
  (void)other;
  return self == other;
}

static char *
nil_tostr (Node *self)
{
  (void)self;
  return STR_LITERAL_DUP ("NIL");
}

// Integer type
static int
integer_eq (Node *self, Node *other)
{
  return type_eq (self, other) && GET_INTEGER (self) == GET_INTEGER (other);
}

static char *
integer_tostr (Node *self)
{
  char str[INTEGER_TYPE_STR_MAX_SIZE];
  size_t n = sizeof (str);

  int result = snprintf (str, n, CINTEGER_TYPE_FMT, GET_INTEGER (self));

  if (result < 0 || (size_t)result >= n)
    return NULL;

  return safe_strndup (str, n);
}

// Symbol type
static int
symbol_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && GET_SYMBOL (self).str == GET_SYMBOL (other).str;
}

static char *
symbol_tostr (Node *self)
{
  const char *str = GET_SYMBOL (self).str;
  size_t len = GET_SYMBOL (self).len;
  return safe_strndup (str, len);
}

// List type
static int
list_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && ((IS_NIL (self) && IS_NIL (other))
             || GET_CONS (self) == GET_CONS (other));
}

static char *
list_tostr (Node *self)
{
  List *list = NULL;
  size_t total = 0;
  Node *cur;

  list = list_xalloc ();

  if (!list)
    return NULL;

  total += list_append_strdup (list, "(");

  for (cur = self; IS_CONS (cur); cur = CDR (cur))
    {
      Node *car = CAR (cur);
      Node *cdr = CDR (cur);

      if (car)
        {
          total += list_append_strdup (list, type (car)->str_fn (car));

          if (CAR (cdr))
            total += list_append_strdup (list, " ");
        }
    }

  if (!IS_NIL (cur))
    {
      total += list_append_strdup (list, ".");
      total += list_append_strdup (list, type (cur)->str_fn (cur));
    }

  total += list_append_strdup (list, ")");

  // merge down into a single str

  char *str = xcalloc (total + 1, sizeof *(str));
  char *dst = str;

  for (size_t i = 0; i < list->count; ++i)
    {
      for (char *src = list->items[i]; (*dst = *src); ++src, ++dst)
        ;
      free (list->items[i]);
    }

  list_free (list);

  return str;
}

// Primitive type
static int
primitive_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && &GET_BUILTIN_FN (self) == &GET_BUILTIN_FN (other);
}

static char *
primitive_tostr (Node *self)
{
  const BuiltinFn *builtin = GET_BUILTIN_FN (self);
  return safe_strndup (builtin->name, strlen (builtin->name));
}

// Lambda type
static int
lambda_eq (Node *self, Node *other)
{
  return type_eq (self, other) && GET_LAMBDA (self) == GET_LAMBDA (other);
}

static char *
lambda_tostr (Node *self)
{
  const char *fmt = "(#LAMBDA %s %s)";

  char *params_str
      = type (GET_LAMBDA_PARAMS (self))->str_fn (GET_LAMBDA_PARAMS (self));
  char *body_str = type (GET_LAMBDA_BODY (self))
                       ->str_fn (GET_LAMBDA_BODY (self)); // FIXME

  size_t params_len = (params_str) ? strlen (params_str) : 0;
  size_t body_len = (body_str) ? strlen (body_str) : 0;

  size_t total = strlen (fmt) + params_len + body_len;

  char *str = xcalloc (total, sizeof *str);

  int result = snprintf (str, total, fmt, params_str, body_str);
  if (result < 0 || (size_t)result >= total)
    {
      free (str);
      return NULL;
    }

  free (params_str);
  free (body_str);

  return str;
}

// String type
static int
string_eq (Node *self, Node *other)
{
  return type_eq (self, other)
         && (!strcmp (GET_STRING (self),
                      GET_STRING (other))); // FIX ME: strings should have len
}

static char *
string_tostr (Node *self)
{
  return GET_STRING (self);
}

static Type type_tab[] = {
  // Special constant
  [TYPE_NIL] = { .type_name = "NIL", .str_fn = nil_tostr, .eq_fn = nil_eq },

  // Literal values
  [TYPE_INTEGER]
  = { .type_name = "INTEGER", .str_fn = integer_tostr, .eq_fn = integer_eq },
  [TYPE_STRING]
  = { .type_name = "STRING", .str_fn = string_tostr, .eq_fn = string_eq },
  [TYPE_SYMBOL]
  = { .type_name = "SYMBOL", .str_fn = symbol_tostr, .eq_fn = symbol_eq },

  // Composite structures
  [TYPE_CONS]
  = { .type_name = "CONS", .str_fn = list_tostr, .eq_fn = list_eq },

  // Function-like values
  [TYPE_BUILTIN_FN] = { .type_name = "PRIMITIVE",
                        .str_fn = primitive_tostr,
                        .eq_fn = primitive_eq },
  [TYPE_LAMBDA]
  = { .type_name = "LAMBDA", .str_fn = lambda_tostr, .eq_fn = lambda_eq },
};

// type()
const Type *
type (Node *self)
{
  if (!self || IS_NIL (self))
    return &type_tab[TYPE_NIL];

  return &type_tab[self->type];
}

Node *
cons_lambda (Pool **p, Node *params, Node *body, Env *env)
{
  Node *node = pool_xalloc_hier (p);
  node->type = TYPE_LAMBDA;
  node->lambda.params = params;
  node->lambda.body = body;
  node->lambda.env = env;
  return node;
}

Node *
cons_integer (Pool **p, Integer i)
{
  Node *node = pool_xalloc_hier (p);
  node->type = TYPE_INTEGER;
  node->integer = i;
  return node;
}

Node *
cons_cons (Pool **p, Node *car, Node *cdr)
{
  Node *node = pool_xalloc_hier (p);
  node->type = TYPE_CONS;
  CAR (node) = car;
  CDR (node) = cdr;
  return node;
}

Node *
cons_string (Pool **p, char *str)
{
  Node *node = pool_xalloc_hier (p);
  node->type = TYPE_STRING;
  node->string = str;
  return node;
}

Node *
cons_symbol (Pool **p, const char *str, size_t len)
{
  Node *node = pool_xalloc_hier (p);
  node->type = TYPE_SYMBOL;
  node->symbol.str = str;
  node->symbol.len = len;
  return node;
}
