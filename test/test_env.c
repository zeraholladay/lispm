#include <check.h>

#include "builtins.h"
#include "lm_env.h"
#include "parser.h"
#include "prims.h"

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

static void
reset_after_api_error (void)
{
  ck_assert (lm->has_error);
  // The evaluator consumes the pending API error and resets its registers.
  EvaluationResult result = lm_evaluate (lm, NIL);
  ck_assert (!result.success);
  ck_assert_ptr_eq (result.value, NIL);
  ck_assert (!lm->has_error);
}

START_TEST (test_environment_lookup_shadowing_and_parent_assignment)
{
  Cell *name         = cell_make_symbol (lm, "binding", 7);
  Cell *global_value = cell_make_integer (lm, 10);
  ck_assert (lm_env_define (lm, name, global_value));
  lm->env.current = cell_make_environment (lm, lm->env.global);
  ck_assert_ptr_eq (lm_env_lookup (lm, name), global_value);
  Cell *updated = cell_make_integer (lm, 20);
  ck_assert (lm_env_set (lm, name, updated));
  ck_assert_ptr_eq (lm_env_lookup (lm, name), updated);
  Cell *local_value = cell_make_integer (lm, 30);
  ck_assert (lm_env_define (lm, name, local_value));
  ck_assert_ptr_eq (lm_env_lookup (lm, name), local_value);
  ck_assert (lm_env_set (lm, name, T));
  ck_assert_ptr_eq (lm_env_lookup (lm, name), T);
  lm->env.current = lm->env.global;
  ck_assert_ptr_eq (lm_env_lookup (lm, name), updated);
  Cell *add = cell_make_symbol (lm, "add", 3);
  ck_assert_ptr_eq (lm_env_lookup (lm, add), builtin_value (BUILTIN_ADD));
}
END_TEST

START_TEST (test_environment_guards_preserve_bindings)
{
  Cell *invalid[]
      = { cell_make_integer (lm, 1), cell_make_symbol (lm, "car", 3) };
  size_t count = lm->env.global->env.dict->count;
  for (size_t i = 0; i < sizeof (invalid) / sizeof (invalid[0]); ++i)
    {
      ck_assert (!lm_env_define (lm, invalid[i], T));
      reset_after_api_error ();
      ck_assert (!lm_env_set (lm, invalid[i], T));
      reset_after_api_error ();
      ck_assert_uint_eq (lm->env.global->env.dict->count, count);
    }
  ck_assert_ptr_eq (lm_env_lookup (lm, invalid[1]),
                    builtin_value (BUILTIN_CAR));
}
END_TEST

START_TEST (test_missing_names_fail_without_creating_bindings)
{
  Cell  *name  = cell_make_symbol (lm, "missing-binding", 15);
  size_t count = lm->env.global->env.dict->count;
  ck_assert (!lm_env_set (lm, name, T));
  reset_after_api_error ();
  ck_assert_ptr_eq (lm_env_lookup (lm, name), NIL);
  reset_after_api_error ();
  ck_assert_uint_eq (lm->env.global->env.dict->count, count);
  ck_assert_ptr_null (
      dict_lookup (lm->env.global->env.dict, "missing-binding"));

  const char *sources[]
      = { "missing-binding", "(set! missing-binding 1)", "(define car 1)" };
  for (size_t i = 0; i < sizeof (sources) / sizeof (sources[0]); ++i)
    {
      Cell *forms = NULL;
      ck_assert (parser_buf (sources[i], &forms, lm));
      EvaluationResult result = lm_evaluate (lm, forms);
      ck_assert (!result.success);
      ck_assert_ptr_eq (result.value, NIL);
      ck_assert (!lm->has_error);
      ck_assert_ptr_eq (lm->env.current, lm->env.global);
      ck_assert (parser_buf ("(+ 20 22)", &forms, lm));
      result = lm_evaluate (lm, forms);
      ck_assert (result.success);
      ck_assert_int_eq (result.value->integer, 42);
    }
}
END_TEST

Suite *
env_suite (void)
{
  Suite *suite = suite_create ("Environment");
  TCase *tc    = tcase_create ("Bindings and errors");
  tcase_add_checked_fixture (tc, setup, teardown);
  tcase_add_test (tc, test_environment_lookup_shadowing_and_parent_assignment);
  tcase_add_test (tc, test_environment_guards_preserve_bindings);
  tcase_add_test (tc, test_missing_names_fail_without_creating_bindings);
  suite_add_tcase (suite, tc);
  return suite;
}
