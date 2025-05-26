#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "debug.h"
#include "error.h"
#include "eval.h"
#include "eval_bool.h"
#include "keywords.h"
#include "parser.h"
#include "safe_str.h"
#include "types.h"

#define PRINT(node)                                                           \
  do                                                                          \
    {                                                                         \
      StrFn to_str_fn = type (node)->str_fn;                                  \
      char *str = to_str_fn (node);                                           \
      printf ("%s\n", str);                                                   \
      free (str);                                                             \
    }                                                                         \
  while (0)

// funcalls
static Node *funcall (Node *fn, Node *arglist, Context *ctx);
static Node *funcall_builtin (Node *fn, Node *args, Context *ctx);
static Node *funcall_lambda (Node *fn, Node *args, Context *ctx);

// cond forms/expressions
static Node *and_form (Node *form, Context *ctx);
static Node *if_form (Node *form, Context *ctx);
static Node *or_form (Node *form, Context *ctx);

// sequence operations
static Node *append_inplace (Node *list1, Node *list2);
static Node *append_list (Node *list1, Node *list2, Context *ctx);
static Node *butlast (Node *args, Context *ctx);
static Node *last (Node *args, Context *ctx);
static size_t length (Node *list);
static Node *mapcar (Node *fn, Node *arglist, Context *ctx);
static Node *nth (size_t idx, Node *list);
static Node *pair (Node *l1, Node *l2, Context *ctx);
static Node *reverse (Node *list, Context *ctx);
static Node *zip (Node *lists, Context *ctx);

// context operations
static Node *lookup (Node *node, Context *ctx);
static Node *set (Node *car, Node *REST, Context *ctx);

// funcall & eval
static Node *
funcall (Node *fn, Node *arglist, Context *ctx)
{
  if (IS_BUILTIN_FN (fn))
    return funcall_builtin (fn, arglist, ctx);

  if (IS_LAMBDA (fn))
    return funcall_lambda (fn, arglist, ctx);

  raise (ERR_NOT_A_FUNCTION, type (fn)->str_fn (fn));
  return NULL;
}

static Node *
funcall_builtin (Node *fn, Node *arglist, Context *ctx)
{
  int received = (int)length (arglist);
  const BuiltinFn *builtin_fn = GET_BUILTIN_FN (fn);

  if (builtin_fn->arity > 0 && builtin_fn->arity != received)
    {
      ErrorCode err = (received < GET_BUILTIN_FN (fn)->arity)
                          ? ERR_MISSING_ARG
                          : ERR_UNEXPECTED_ARG;
      raise (err, builtin_fn->name);
      return NULL;
    }

  // eval_apply or eval_funcall could have taken us here.
  // so if we called them again, arglist would be eval'd 2x.
  if (fn == KEYWORD (FUNCALL))
    {
      Node *fn2 = eval (FIRST (arglist), ctx);
      return funcall (fn2, REST (arglist), ctx);
    }

  if (fn == KEYWORD (APPLY))
    {
      Node *fn2 = eval (FIRST (arglist), ctx);
      return funcall (fn2, FIRST (REST (arglist)), ctx);
    }

  if (fn == KEYWORD (LIST))
    return arglist; // LIST is eval_list, so we're done.

  return builtin_fn->fn (arglist, ctx);
}

static Node *
funcall_lambda (Node *fn, Node *args, Context *ctx)
{
  size_t expected = length (GET_LAMBDA_PARAMS (fn));
  size_t received = length (args);

  if (expected != received)
    {
      ErrorCode err
          = (received < expected) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
      raise (err, "funcall/lambda");
      return NULL;
    }

  Context new_ctx = *ctx;
  CTX_ENV (&new_ctx) = env_new (GET_LAMBDA_ENV (fn));

  Node *pairs = pair (GET_LAMBDA_PARAMS (fn), args, ctx);

  while (!IS_NIL (pairs))
    {
      Node *pair = FIRST (pairs);
      set (FIRST (pair), FIRST (REST (pair)), &new_ctx);
      pairs = REST (pairs);
    }

  return eval_progn (GET_LAMBDA_BODY (fn), &new_ctx);
}

// (apply f arglist)
// (define (apply f . args)
//   (let* ((fixed-args   (butlast args))   ; all but the last element
//          (last-arg-list (last args))     ; the final element, as a list
//          (all-args     (append fixed-args last-arg-list)))
//     (funcall f all-args)))
Node *
eval_apply (Node *arglist, Context *ctx)
{
  Node *fn = eval (FIRST (arglist), ctx);

  Node *fixed_rev = NIL;
  Node *fixd_args = butlast (REST (arglist), ctx);

  while (!IS_NIL (fixd_args))
    {
      Node *eval_res = eval (FIRST (fixd_args), ctx);
      fixed_rev = CONS (eval_res, fixed_rev, ctx);
      fixd_args = REST (fixd_args);
    }

  Node *last_arg_list = last (REST (arglist), ctx);
  Node *tail_list = eval (last_arg_list, ctx);

  if (!LISTP (tail_list))
    {
      raise (ERR_INVALID_ARG, "apply");
      return NULL;
    }

  Node *fixed = reverse (fixed_rev, ctx);
  Node *all = append_inplace (fixed, tail_list);

  return funcall (fn, all, ctx);
}

Node *
eval_funcall (Node *args, Context *ctx)
{
  Node *fn = eval (FIRST (args), ctx);
  Node *arglist = eval_list (REST (args), ctx);
  return funcall (fn, arglist, ctx);
}

Node *
eval_list (Node *args, Context *ctx)
{
  if (IS_NIL (args))
    return NIL;

  Node *first = eval (FIRST (args), ctx);
  Node *rest = eval_list (REST (args), ctx);

  return CONS (first, rest, ctx);
}

Node *
eval (Node *form, Context *ctx)
{
  // SYMBOLS
  if (IS_SYMBOL (form))
    return lookup (form, ctx);

  // LITERALS: NUMBERS, STRINGS, ETC.
  if (!LISTP (form))
    return form;

  if (LISTP (form))
    {
      if (IS_NIL (form))
        return NIL;

      Node *first = FIRST (form);
      Node *rest = REST (form);

      if (first == KEYWORD (QUOTE))
        return FIRST (rest);

      if (IS_LAMBDA (first))
        {
          GET_LAMBDA_ENV (first) = CTX_ENV (ctx);
          return first;
        }

      Node *fn = eval (first, ctx);

      if (IS_SPECIAL_FORM (fn))
        {
          if (fn == KEYWORD (APPLY))
            return eval_apply (rest, ctx);

          if (fn == KEYWORD (FUNCALL))
            return eval_funcall (rest, ctx);

          if (fn == KEYWORD (EVAL))
            return eval (eval (FIRST (rest), ctx), ctx);

          if (fn == KEYWORD (PROGN))
            return eval_progn (rest, ctx);

          if (fn == KEYWORD (AND))
            return and_form (rest, ctx);

          if (fn == KEYWORD (IF))
            return if_form (rest, ctx);

          if (fn == KEYWORD (OR))
            return or_form (rest, ctx);

          raise (ERR_INTERNAL, DEBUG_LOCATION);
          return NULL;
        }

      Node *arglist = eval_list (rest, ctx);
      return funcall (fn, arglist, ctx);
    }

  raise (ERR_INTERNAL, DEBUG_LOCATION);
  return NULL;
}

Node *
eval_progn (Node *program, Context *ctx)
{
  Node *result = NIL;

  for (Node *forms = program; forms != NIL; forms = REST (forms))
    {
      Node *form = FIRST (forms);
      result = eval (form, ctx);
    }

  return result;
}

// conditional forms/expressions
static Node *
and_form (Node *form, Context *ctx)
{
  Node *eval_res = T;
  EqFn nil_eq_fn = type (NIL)->eq_fn;

  while (!IS_NIL (form))
    {
      eval_res = eval (FIRST (form), ctx);
      if (nil_eq_fn (NIL, eval_res))
        {
          return NIL;
        }
      form = REST (form);
    }

  return eval_res;
}

static Node *
if_form (Node *form, Context *ctx)
{
  Node *pred_form = FIRST (form);

  if (!IS_NIL (eval (pred_form, ctx)))
    return eval (FIRST (REST (form)), ctx);
  else
    {
      Node *else_form = FIRST (REST (REST (form)));
      return else_form ? eval (else_form, ctx) : NIL;
    }
}

static Node *
or_form (Node *form, Context *ctx)
{
  Node *eval_res = NIL;
  EqFn nil_eq_fn = type (NIL)->eq_fn;

  while (!IS_NIL (form))
    {
      eval_res = eval (FIRST (form), ctx);

      if (!nil_eq_fn (NIL, eval_res))
        {
          return eval_res;
        }

      form = REST (form);
    }
  return NIL;
}

// sequence operations

static Node *
append_inplace (Node *list1, Node *list2)
{
  if (IS_NIL (list1))
    return list2;

  Node *l1 = list1;

  while (!IS_NIL (REST (l1)))
    l1 = REST (l1);

  RPLACD (l1, list2);

  return list1;
}

static Node *
append_list (Node *list1, Node *list2, Context *ctx)
{
  if (IS_NIL (list1))
    return list2;

  return CONS (FIRST (list1), append_list (REST (list1), list2, ctx), ctx);
}

static Node *
butlast (Node *list, Context *ctx)
{
  Node *rev = reverse (list, ctx);
  return reverse (REST (rev), ctx);
}

static Node *
last (Node *list, Context *ctx)
{
  Node *rev = reverse (list, ctx);
  return FIRST (rev);
}

static size_t
length (Node *list)
{
  if (!IS_LIST (list))
    return 0;

  size_t i = 1;

  for (Node *cdr = REST (list); cdr != NIL; cdr = REST (cdr))
    ++i;

  return i;
}

static Node *
mapcar (Node *fn, Node *arglist, Context *ctx)
{
  Node *zip_args = zip (arglist, ctx);
  Node *rev = NIL;

  for (Node *l = zip_args; !IS_NIL (l); l = REST (l))
    {
      Node *res = funcall (fn, FIRST (l), ctx);
      rev = CONS (res, rev, ctx);
    }

  return reverse (rev, ctx);
}

static Node *
pair (Node *list1, Node *list2, Context *ctx)
{
  if (IS_NIL (list1) || IS_NIL (list2))
    return NIL;

  Node *first_pair = LIST2 (FIRST (list1), FIRST (list2), ctx);
  Node *rest_pairs = pair (REST (list1), REST (list2), ctx);

  return CONS (first_pair, rest_pairs, ctx);
}

static Node *
nth (size_t idx, Node *list)
{
  for (size_t i = 0; i < idx; ++i)
    {
      if (IS_NIL (list))
        return NIL;
      list = REST (list);
    }

  return (IS_NIL (list)) ? NIL : FIRST (list);
}

static Node *
reverse (Node *list, Context *ctx)
{
  (void)ctx;
  Node *result = NIL;

  for (Node *l = list; l != NIL; l = REST (l))
    {
      result = CONS (FIRST (l), result, ctx);
    }
  return result;
}

static Node *
zip (Node *lists, Context *ctx)
{
  size_t len = length (lists);
  if (len == 0)
    return NIL;

  Node **heads = calloc (len, sizeof *heads);
  if (!heads)
    {
      return NIL; // FIXME
    }

  for (size_t i = 0; i < len; i++)
    heads[i] = nth (i, lists);

  Node *out_rev = NIL;

  for (;;)
    {
      int done = 0;

      for (size_t i = 0; i < len; i++)
        if (IS_NIL (heads[i]))
          {
            done = 1;
            break;
          }
      if (done)
        break;

      Node *row_rev = NIL;

      for (size_t i = 0; i < len; i++)
        {
          row_rev = CONS (FIRST (heads[i]), row_rev, ctx);
          heads[i] = REST (heads[i]);
        }

      out_rev = CONS (reverse (row_rev, ctx), out_rev, ctx);
    }

  free (heads);
  return reverse (out_rev, ctx);
}

// context operations
static Node *
lookup (Node *node, Context *ctx)
{
  const char *str = GET_SYMBOL (node).str;
  size_t len = GET_SYMBOL (node).len;

  Node *kywrd_node = keyword_lookup (str, len);
  if (kywrd_node)
    {
      return kywrd_node;
    }

  rb_node *n = env_lookup (CTX_ENV (ctx), str);
  if (!n)
    {
      raise (ERR_SYMBOL_NOT_FOUND, str);
      return NULL;
    }

  return RB_VAL (n);
}

static Node *
set (Node *first, Node *rest, Context *ctx)
{
  if (!IS_SYMBOL (first))
    {
      raise (ERR_INVALID_ARG, "set");
      return NULL;
    }

  const char *str = GET_SYMBOL (first).str;
  size_t len = GET_SYMBOL (first).len;

  if (keyword_lookup (str, len))
    {
      raise (ERR_INVALID_ARG, "set");
      return NULL;
    }

  env_set (CTX_ENV (ctx), str, rest); // TODO: error handling
  return rest;
}

// other builtins

Node *
eval_append (Node *args, Context *ctx)
{
  if (!LISTP (args))
    {
      raise (ERR_INVALID_ARG, "append");
      return NULL;
    }

  Node *result = FIRST (args);

  for (Node *list = REST (args); args != NIL; args = REST (args))
    {
      result = append_list (result, FIRST (list), ctx);
    }

  return result;
}

Node *
eval_butlast (Node *args, Context *ctx)
{
  if (!LISTP (args))
    {
      raise (ERR_INVALID_ARG, "butlast");
      return NULL;
    }
  return butlast (FIRST (args), ctx);
}

Node *
eval_cons (Node *args, Context *ctx)
{
  return CONS (FIRST (args), FIRST (REST (args)), ctx);
}

Node *
eval_first (Node *args, Context *ctx)
{
  (void)ctx;
  if (!LISTP (FIRST (args)))
    {
      raise (ERR_INVALID_ARG, "first");
      return NULL;
    }
  return FIRST (FIRST (args));
}

Node *
eval_len (Node *args, Context *ctx)
{
  Node *first = FIRST (args);

  if (!LISTP (args))
    {
      raise (ERR_INVALID_ARG, "len");
      return NULL;
    }
  return cons_integer (&CTX_POOL (ctx), length (first));
}

Node *
eval_mapcar (Node *args, Context *ctx)
{
  Node *fn = FIRST (args);
  Node *arglist = REST (args);
  return mapcar (fn, arglist, ctx);
}

Node *
eval_nth (Node *args, Context *ctx)
{
  (void)ctx;

  if (!IS_LIST (args) || !IS_INTEGER (FIRST (args))
      || !LISTP (FIRST (REST (args))))
    {
      raise (ERR_ARG_NOT_ITERABLE, "nth: i list");
      return NULL;
    }
  size_t idx = (size_t)GET_INTEGER (FIRST (args));
  Node *list = FIRST (REST (args));
  return nth (idx, list);
}

Node *
eval_last (Node *args, Context *ctx)
{
  if (!LISTP (args))
    {
      raise (ERR_INVALID_ARG, "last");
      return NULL;
    }
  return last (FIRST (args), ctx);
}

Node *
eval_pair (Node *args, Context *ctx)
{
  if (!LISTP (FIRST (args)) || !LISTP (FIRST (REST (args))))
    {
      raise (ERR_INVALID_ARG, "pair");
      return NULL;
    }
  return pair (FIRST (args), FIRST (REST (args)), ctx);
}

Node *
eval_print (Node *args, Context *ctx)
{
  (void)ctx;
  if (!LISTP (args))
    {
      raise (ERR_INVALID_ARG, "print");
      return NULL;
    }
  PRINT (FIRST (args));
  return T;
}

Node *
eval_rest (Node *args, Context *ctx)
{
  (void)ctx;
  Node *first = FIRST (args);

  if (!LISTP (first))
    {
      raise (ERR_INVALID_ARG, "rest");
      return NULL;
    }
  return REST (first);
}

Node *
eval_reverse (Node *args, Context *ctx)
{
  if (!LISTP (args))
    {
      raise (ERR_INVALID_ARG, "rest");
      return NULL;
    }
  return reverse (FIRST (args), ctx);
}

Node *
eval_set (Node *args, Context *ctx)
{
  if (!IS_SYMBOL (FIRST (args)))
    {
      raise (ERR_INVALID_ARG, "set");
      return NULL;
    }
  return set (FIRST (args), FIRST (REST (args)), ctx);
}

Node *
eval_str (Node *args, Context *ctx)
{
  return cons_string (&CTX_POOL (ctx), type (args)->str_fn (args));
}
