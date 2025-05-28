#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "eval.h"
#include "format.h"
#include "xalloc.h"

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
static Node *reverse (Node *list, Context *ctx);
static Node *reverse_inplace (Node *list);
static Node *zip (Node *lists, Context *ctx);

// context operations
static Node *lookup (Node *node, Context *ctx);
static Node *set (Node *car, Node *REST, Context *ctx);

// funcall & eval
static Node *
funcall (Node *fn, Node *arglist, Context *ctx)
{
  if (IS (fn, BUILTIN_FN))
    return funcall_builtin (fn, arglist, ctx);

  if (IS (fn, LAMBDA))
    return funcall_lambda (fn, arglist, ctx);

  raise (ERR_NOT_A_FUNCTION, DEBUG_LOCATION);
  return NULL;
}

static Node *
funcall_builtin (Node *fn, Node *arglist, Context *ctx)
{
  int received = (int)length (arglist);
  const BuiltinFn *builtin_fn = fn->builtin_fn;

  if (builtin_fn->arity > 0 && builtin_fn->arity != received)
    {
      ErrorCode err = (received < builtin_fn->arity) ? ERR_MISSING_ARG
                                                     : ERR_UNEXPECTED_ARG;
      raise (err, builtin_fn->name);
      return NULL;
    }

  // eval_apply or eval_funcall could have taken us here.
  // so if we called them again, arglist would be eval'd 2x.
  if (fn == KEYWORD (FUNCALL))
    {
      Node *fn2 = eval (CAR (arglist), ctx);
      return funcall (fn2, CDR (arglist), ctx);
    }

  if (fn == KEYWORD (APPLY))
    {
      Node *fn2 = eval (CAR (arglist), ctx);
      return funcall (fn2, CAR (CDR (arglist)), ctx);
    }

  if (fn == KEYWORD (LIST))
    return arglist; // LIST is eval_list, so we're done.

  return builtin_fn->fn (arglist, ctx);
}

static Node *
funcall_lambda (Node *fn, Node *args, Context *ctx)
{
  size_t expected = length (fn->lambda.params);
  size_t received = length (args);

  if (expected != received)
    {
      ErrorCode err
          = (received < expected) ? ERR_MISSING_ARG : ERR_UNEXPECTED_ARG;
      raise (err, "funcall/lambda");
      return NULL;
    }

  env_enter_frame (&ctx->env);

  Node *pairs
      = mapcar (KEYWORD (LIST), LIST2 (fn->lambda.params, args, ctx), ctx);

  while (!IS_NIL (pairs))
    {
      Node *pair = CAR (pairs);
      env_let (ctx->env, (CAR (pair))->symbol.str, CADR (pair));
      pairs = CDR (pairs);
    }

  Node *res = eval_progn (fn->lambda.body, ctx);

  env_leave_frame (&ctx->env);

  return res;
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
  Node *fn = eval (CAR (arglist), ctx);

  Node *fixed_rev = NIL;
  Node *fixd_args = butlast (CDR (arglist), ctx);

  while (!IS_NIL (fixd_args))
    {
      Node *eval_res = eval (CAR (fixd_args), ctx);
      fixed_rev = CONS (eval_res, fixed_rev, ctx);
      fixd_args = CDR (fixd_args);
    }

  Node *last_arg_list = last (CDR (arglist), ctx);
  Node *tail_list = eval (last_arg_list, ctx);

  if (!LISTP (tail_list))
    {
      raise (ERR_INVALID_ARG, "apply");
      return NULL;
    }

  Node *fixed = reverse_inplace (fixed_rev);
  Node *all = append_inplace (fixed, tail_list);

  return funcall (fn, all, ctx);
}

Node *
eval_funcall (Node *args, Context *ctx)
{
  Node *fn = eval (CAR (args), ctx);
  Node *arglist = eval_list (CDR (args), ctx);
  return funcall (fn, arglist, ctx);
}

Node *
eval_list (Node *args, Context *ctx)
{
  if (IS_NIL (args))
    return NIL;

  Node *car = eval (CAR (args), ctx);
  Node *cdr = eval_list (CDR (args), ctx);

  return CONS (car, cdr, ctx);
}

Node *
eval (Node *form, Context *ctx)
{
  if (IS (form, SYMBOL))
    return lookup (form, ctx);

  // LITERALS: NUMBERS, STRINGS, ETC.
  if (!LISTP (form))
    return form;

  if (LISTP (form))
    {
      if (IS_NIL (form))
        return NIL;

      Node *car = CAR (form);
      Node *cdr = CDR (form);

      if (car == KEYWORD (QUOTE))
        return CAR (cdr);

      if (IS (car, LAMBDA))
        return car;

      Node *fn = eval (car, ctx);

      if (IS (fn, BUILTIN_FN) && fn->builtin_fn->sform)
        {
          if (fn == KEYWORD (APPLY))
            return eval_apply (cdr, ctx);

          if (fn == KEYWORD (FUNCALL))
            return eval_funcall (cdr, ctx);

          if (fn == KEYWORD (EVAL))
            return eval (eval (CAR (cdr), ctx), ctx);

          if (fn == KEYWORD (PROGN))
            return eval_progn (cdr, ctx);

          if (fn == KEYWORD (AND))
            return and_form (cdr, ctx);

          if (fn == KEYWORD (IF))
            return if_form (cdr, ctx);

          if (fn == KEYWORD (OR))
            return or_form (cdr, ctx);

          raise (ERR_INTERNAL, DEBUG_LOCATION);
          return NULL;
        }

      Node *arglist = eval_list (cdr, ctx);
      return funcall (fn, arglist, ctx);
    }

  raise (ERR_INTERNAL, DEBUG_LOCATION);
  return NULL;
}

Node *
eval_progn (Node *program, Context *ctx)
{
  Node *result = NIL;

  for (Node *forms = program; forms != NIL; forms = CDR (forms))
    {
      Node *form = CAR (forms);
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
      eval_res = eval (CAR (form), ctx);
      if (nil_eq_fn (NIL, eval_res))
        {
          return NIL;
        }
      form = CDR (form);
    }

  return eval_res;
}

static Node *
if_form (Node *form, Context *ctx)
{
  Node *pred_form = CAR (form);

  if (!IS_NIL (eval (pred_form, ctx)))
    return eval (CAR (CDR (form)), ctx);
  else
    {
      Node *else_form = CAR (CDR (CDR (form)));
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
      eval_res = eval (CAR (form), ctx);

      if (!nil_eq_fn (NIL, eval_res))
        {
          return eval_res;
        }

      form = CDR (form);
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

  while (!IS_NIL (CDR (l1)))
    l1 = CDR (l1);

  RPLACD (l1, list2);

  return list1;
}

static Node *
append_list (Node *list1, Node *list2, Context *ctx)
{
  if (IS_NIL (list1))
    return list2;

  return CONS (CAR (list1), append_list (CDR (list1), list2, ctx), ctx);
}

static Node *
butlast (Node *list, Context *ctx)
{
  Node *rev = reverse_inplace (list);
  Node *btl = reverse (CDR (rev), ctx);
  reverse_inplace (rev);
  return btl;
}

static Node *
last (Node *list, Context *ctx)
{
  (void)ctx;
  Node *rev = reverse_inplace (list);
  Node *last = CAR (rev);
  reverse_inplace (rev);
  return last;
}

static size_t
length (Node *list)
{
  if (!IS (list, CONS))
    return 0;

  size_t i = 1;

  for (Node *cdr = CDR (list); cdr != NIL; cdr = CDR (cdr))
    ++i;

  return i;
}

static Node *
mapcar (Node *fn, Node *arglist, Context *ctx)
{
  Node *zip_args = zip (arglist, ctx);
  Node *rev = NIL;

  for (Node *l = zip_args; !IS_NIL (l); l = CDR (l))
    {
      Node *res = funcall (fn, CAR (l), ctx);
      rev = CONS (res, rev, ctx);
    }

  return reverse_inplace (rev);
}

static Node *
nth (size_t idx, Node *list)
{
  for (size_t i = 0; i < idx; ++i)
    {
      if (IS_NIL (list))
        return NIL;
      list = CDR (list);
    }

  return (IS_NIL (list)) ? NIL : CAR (list);
}

static Node *
reverse (Node *list, Context *ctx)
{
  (void)ctx;
  Node *result = NIL;

  for (Node *l = list; l != NIL; l = CDR (l))
    result = CONS (CAR (l), result, ctx);

  return result;
}

static Node *
reverse_inplace (Node *list)
{
  Node *prev = NIL;
  Node *cur = list;

  while (!IS_NIL (cur))
    {
      Node *next = CDR (cur);
      RPLACD (cur, prev);
      prev = cur;
      cur = next;
    }

  return prev;
}

static Node *
zip (Node *lists, Context *ctx)
{
  scratch_t s;

  size_t len = length (lists);
  if (len == 0)
    return NIL;

  Node **heads = xalloc_scratch (&s, len * sizeof *heads);

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
          row_rev = CONS (CAR (heads[i]), row_rev, ctx);
          heads[i] = CDR (heads[i]);
        }

      out_rev = CONS (reverse_inplace (row_rev), out_rev, ctx);
    }

  xfree_scratch (&s);
  return reverse_inplace (out_rev);
}

// context operations
static Node *
lookup (Node *node, Context *ctx)
{
  const char *key = node->symbol.str;
  size_t len = node->symbol.len;

  Node *kywrd_node = keyword_lookup (key, len);
  if (kywrd_node)
    return kywrd_node;

  Node *res = env_lookup (ctx->env, key);
  if (!res)
    {
      raise (ERR_SYMBOL_NOT_FOUND, key);
      return NULL;
    }

  return res;
}

static Node *
set (Node *car, Node *cdr, Context *ctx)
{
  if (!IS (car, SYMBOL))
    {
      raise (ERR_INVALID_ARG, "set");
      return NULL;
    }

  const char *key = car->symbol.str;
  size_t len = car->symbol.len;

  if (keyword_lookup (key, len))
    {
      raise (ERR_INVALID_ARG, "set");
      return NULL;
    }

  env_set (ctx->env, key, cdr);
  return cdr;
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

  Node *result = CAR (args);

  for (Node *list = CDR (args); args != NIL; args = CDR (args))
    {
      result = append_list (result, CAR (list), ctx);
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
  return butlast (CAR (args), ctx);
}

Node *
eval_cons (Node *args, Context *ctx)
{
  return CONS (CAR (args), CAR (CDR (args)), ctx);
}

Node *
eval_car (Node *args, Context *ctx)
{
  (void)ctx;
  if (!LISTP (CAR (args)))
    {
      raise (ERR_INVALID_ARG, "car");
      return NULL;
    }
  return CAR (CAR (args));
}

Node *
eval_cdr (Node *args, Context *ctx)
{
  (void)ctx;
  Node *car = CAR (args);

  if (!LISTP (car))
    {
      raise (ERR_INVALID_ARG, "cdr");
      return NULL;
    }
  return CDR (car);
}

Node *
eval_len (Node *args, Context *ctx)
{
  Node *car = CAR (args);

  if (!LISTP (args))
    {
      raise (ERR_INVALID_ARG, "len");
      return NULL;
    }
  return cons_integer (&CTX_POOL (ctx), length (car));
}

Node *
eval_mapcar (Node *args, Context *ctx)
{
  Node *fn = CAR (args);
  Node *arglist = CDR (args);
  return mapcar (fn, arglist, ctx);
}

Node *
eval_nth (Node *args, Context *ctx)
{
  (void)ctx;

  if (!IS (args, CONS) || !IS (CAR (args), INTEGER)
      || !LISTP (CAR (CDR (args))))
    {
      raise (ERR_ARG_NOT_ITERABLE, "nth: i list");
      return NULL;
    }
  size_t idx = (size_t)CAR (args)->integer;
  Node *list = CAR (CDR (args));
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
  return last (CAR (args), ctx);
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
  PRINT (CAR (args));
  return T;
}

Node *
eval_reverse (Node *args, Context *ctx)
{
  if (!LISTP (args))
    {
      raise (ERR_INVALID_ARG, "cdr");
      return NULL;
    }
  return reverse (CAR (args), ctx);
}

Node *
eval_set (Node *args, Context *ctx)
{
  if (!IS (CAR (args), SYMBOL))
    {
      raise (ERR_INVALID_ARG, "set");
      return NULL;
    }
  return set (CAR (args), CAR (CDR (args)), ctx);
}

Node *
eval_string (Node *args, Context *ctx)
{
  return cons_string (&CTX_POOL (ctx), format (FIRST (args)));
}
