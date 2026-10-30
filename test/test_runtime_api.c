#include <check.h>
#include <stdlib.h>
#include <string.h>

#include "builtins.h"
#include "keywords.h"
#include "lm.h"
#include "lm_gc.h"
#include "parser.h"
#include "prims.h"
#include "xalloc.h"

static LM *lm;

static void
setup (void)
{
  lm = lm_create ();
  ck_assert_ptr_nonnull (lm);
}

static void
teardown (void)
{
  lm_destroy (lm);
}

START_TEST (test_predicate_arguments_are_evaluated_once)
{
  Cell  *values[] = { NIL, cell_make_integer (lm, 42), cell_list1 (lm, NIL) };
  size_t index    = 0;
  ck_assert (cell_is_pair_or_nil (values[index++]));
  ck_assert_int_eq (index, 1);
  ck_assert (cell_has_type (values[index++], CELL_TYPE_INTEGER));
  ck_assert_int_eq (index, 2);
  ck_assert (cell_is_pair (values[index++]));
  ck_assert_int_eq (index, 3);
  ck_assert (!cell_is_nil (NULL));
  ck_assert (!cell_is_pair_or_nil (NULL));
  ck_assert (!cell_has_type (NULL, CELL_TYPE_INTEGER));
}
END_TEST

START_TEST (test_builtin_objects_have_canonical_identity)
{
  Cell *add = builtin_value (BUILTIN_ADD);
  ck_assert_ptr_eq (add, keyword_lookup ("+", 1));
  ck_assert_ptr_eq (add, keyword_lookup ("add", 3));
  ck_assert_str_eq (builtin_name (add), "ADD");
  ck_assert (!builtin_is_special_form (add));
  ck_assert (builtin_is_special_form (builtin_value (BUILTIN_IF)));
  ck_assert (!builtin_is_special_form (NULL));
  ck_assert_ptr_null (builtin_value (BUILTIN_COUNT));
  ck_assert_ptr_null (builtin_value ((BuiltinId)-1));

  LM *other = lm_create ();
  ck_assert_ptr_nonnull (other);
  lm_gc (lm);
  lm_gc (other);
  ck_assert_ptr_eq (add, builtin_value (BUILTIN_ADD));
  lm_destroy (other);
  ck_assert_ptr_eq (add, keyword_lookup ("+", 1));
}
END_TEST

START_TEST (test_checked_evaluation_distinguishes_nil_and_error)
{
  Cell *forms = NULL;
  ck_assert (parser_buf ("NIL", &forms, lm));
  EvaluationResult result = lm_evaluate (lm, forms);
  ck_assert (result.success);
  ck_assert_ptr_eq (result.value, NIL);

  ck_assert (parser_buf ("(error 'requested-failure)", &forms, lm));
  result = lm_evaluate (lm, forms);
  ck_assert (!result.success);
  ck_assert_ptr_eq (result.value, NIL);
  ck_assert (!lm->has_error);
  ck_assert_int_eq (lm->ctl.sp, 0);
  ck_assert_int_eq (lm->stk.sp, 0);

  ck_assert (parser_buf ("(+ 20 22)", &forms, lm));
  result = lm_evaluate (lm, forms);
  ck_assert (result.success);
  ck_assert_int_eq (result.value->integer, 42);
}
END_TEST

START_TEST (test_string_take_ownership_and_gc)
{
  char *owned = xmalloc (6);
  memcpy (owned, "owned", 6);
  Cell *value = cell_make_string_take (lm, owned);
  ck_assert_ptr_eq (value->string, owned);
  lm->stk.cells[lm->stk.sp++] = value;
  lm_gc (lm);
  ck_assert (!pool_gc_is_free (value));
  ck_assert_str_eq (value->string, "owned");
  --lm->stk.sp;
  lm_gc (lm);
  ck_assert (pool_gc_is_free (value));
}
END_TEST

Suite *
runtime_api_suite (void)
{
  Suite *suite = suite_create ("Runtime API");
  TCase *tc    = tcase_create ("Conventions");
  tcase_add_checked_fixture (tc, setup, teardown);
  tcase_add_test (tc, test_predicate_arguments_are_evaluated_once);
  tcase_add_test (tc, test_builtin_objects_have_canonical_identity);
  tcase_add_test (tc, test_checked_evaluation_distinguishes_nil_and_error);
  tcase_add_test (tc, test_string_take_ownership_and_gc);
  suite_add_tcase (suite, tc);
  return suite;
}
