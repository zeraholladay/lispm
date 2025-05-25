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

static Node *append_inplace (Node *list1, Node *list2);
static Node *append_list (Node *list1, Node *list2, Context *ctx);
static Node *funcall (Node *fn, Node *arglist, Context *ctx);
static Node *funcall_builtin (Node *fn, Node *args, Context *ctx);
static Node *funcall_lambda (Node *fn, Node *args, Context *ctx);
static size_t length (Node *list);
static Node *lookup (Node *node, Context *ctx);
static Node *pair (Node *l1, Node *l2, Context *ctx);
static Node *reverse (Node *list, Context *ctx);
static Node *set (Node *car, Node *REST, Context *ctx);

Node *
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

Node *
append_list (Node *list1, Node *list2, Context *ctx)
{
  if (IS_NIL (list1))
    return list2;

  return CONS (FIRST (list1), append_list (REST (list1), list2, ctx), ctx);
}

// (funcall f arg1 arg2 ...)
static Node *
funcall (Node *fn, Node *arglist, Context *ctx)
{
  // (fun arg1 arg2 ... )
  if (IS_BUILTIN_FN (fn))
    return funcall_builtin (fn, arglist, ctx);

  // (fun arg1 arg2 ... )
  if (IS_LAMBDA (fn))
    return funcall_lambda (fn, arglist, ctx);

  DEBUG (DEBUG_LOCATION);
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

  // LIST is eval_list, so we're done.
  if (fn == KEYWORD (LIST))
    return arglist;

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

  return eval_program (GET_LAMBDA_BODY (fn), &new_ctx);
}

static size_t
length (Node *list)
{
  size_t i = 0;

  for (Node *cdr = REST (list); cdr; cdr = REST (cdr))
    ++i;

  return i;
}

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
pair (Node *list1, Node *list2, Context *ctx)
{
  if (IS_NIL (list1) || IS_NIL (list2))
    return NIL;

  Node *first_pair = LIST2 (FIRST (list1), FIRST (list2), ctx);
  Node *rest_pairs = pair (REST (list1), REST (list2), ctx);

  return CONS (first_pair, rest_pairs, ctx);
}

Node *
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

// (apply f arglist)
Node *
eval_apply (Node *arglist, Context *ctx)
{
  Node *fn = eval (FIRST (arglist), ctx);

  Node *fixed_rev = NIL;
  Node *walk = REST (arglist);

  // FIXME: mapcar, last, butlast
  while (REST (walk) != NIL)
    {
      fixed_rev = CONS (eval (FIRST (walk), ctx), fixed_rev, ctx);
      walk = REST (walk);
    }

  Node *tail_list = eval (FIRST (walk), ctx);

  if (!LISTP (tail_list))
    {
      raise (ERR_INVALID_ARG, "apply");
      return NULL;
    }

  Node *fixed = reverse (fixed_rev, ctx);
  Node *all = append_list (fixed, tail_list, ctx);
  // FIXME END

  return funcall (fn, all, ctx);
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

  Node *first = FIRST (FIRST (args));

  return first ? first : NIL;
}

Node *
eval_funcall (Node *args, Context *ctx)
{
  Node *fn = eval (FIRST (args), ctx);
  Node *arglist = eval_list (REST (args), ctx);
  return funcall (fn, arglist, ctx);
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
eval_list (Node *args, Context *ctx)
{
  if (IS_NIL (args))
    return NIL;

  Node *first = eval (FIRST (args), ctx);
  Node *rest = eval_list (REST (args), ctx);

  return CONS (first, rest, ctx);
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

  Node *rest = REST (first);

  return rest ? rest : NIL;
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
        {
          return FIRST (rest);
        }

      if (first == KEYWORD (IF))
        {
          Node *pred_form = FIRST (rest);

          if (!IS_NIL (eval (pred_form, ctx)))
            {
              return eval (FIRST (REST (rest)), ctx);
            }
          else
            {
              Node *else_form = FIRST (REST (REST (rest)));
              return else_form ? eval (else_form, ctx) : NIL;
            }
        }

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

          if (fn == KEYWORD (AND))
            return eval_and (rest, ctx);

          if (fn == KEYWORD (OR))
            return eval_or (rest, ctx);

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
eval_program (Node *program, Context *ctx)
{
  Node *result = NIL;
  for (Node *forms = program; forms != NIL; forms = REST (forms))
    {
      Node *form = FIRST (forms);
      result = eval (form, ctx);
    }

  return result;
}
