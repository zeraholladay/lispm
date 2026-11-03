#include <check.h>
#include <stdlib.h>
#include <string.h>

#include "lm.h"
#include "lm_gc.h"
#include "parser.h"
#include "prims.h"

static LM *lm;

static void
setup (void)
{
  lm = lm_create ();
  ck_assert_ptr_nonnull (lm);
  lm_gc (lm);
}

static void
teardown (void)
{
  lm_destroy (lm);
}

START_TEST (test_nonallocating_steps_do_not_collect)
{
  // Many VM steps, but no allocations once this sequence has been built.
  Cell *forms = NIL;
  for (size_t i = 0; i < 1024; ++i)
    forms = cell_make_pair (lm, T, forms);
  lm->stk.cells[lm->stk.sp++] = forms;
  lm_gc (lm);
  --lm->stk.sp;
  size_t           collections = lm->gc.collections;
  EvaluationResult result      = lm_evaluate (lm, forms);
  ck_assert (result.success);
  ck_assert_ptr_eq (result.value, T);
  ck_assert_uint_eq (lm->gc.collections, collections);
  ck_assert_uint_eq (lm->gc.allocations_since_collection, 0);
}
END_TEST

START_TEST (test_pressure_accumulates_across_evaluations)
{
  size_t collections = lm->gc.collections;
  size_t budget      = lm->gc.allocation_budget;
  // Each submission allocates just one sequence cell. Resetting pressure on
  // every evaluation would let such short-lived allocations escape collection.
  for (size_t i = 0; i < budget; ++i)
    {
      Cell            *forms  = cell_list1 (lm, T);
      EvaluationResult result = lm_evaluate (lm, forms);
      ck_assert (result.success);
      ck_assert_ptr_eq (result.value, T);
      ck_assert_uint_eq (lm->gc.collections,
                         collections + (i + 1 == budget ? 1 : 0));
    }
  ck_assert_uint_eq (lm->gc.allocations_since_collection, 0);
}
END_TEST

START_TEST (test_parser_pressure_waits_for_rooted_evaluation)
{
  size_t collections = lm->gc.collections;
  size_t count       = lm->gc.allocation_budget / 2 + 16;
  // Quoted elements each allocate a number and a pair, exceeding the budget
  // while the parser is holding partially constructed objects in C locals.
  char *source = malloc (count * 2 + 4);
  ck_assert_ptr_nonnull (source);
  memcpy (source, "'(", 2);
  for (size_t i = 0; i < count; ++i)
    memcpy (source + 2 + i * 2, "1 ", 2);
  memcpy (source + 2 + count * 2, ")", 2);
  Cell *garbage = cell_make_string_copy (lm, "not rooted");
  Cell *forms   = NULL;
  ck_assert (parser_buf (source, &forms, lm));
  free (source);
  ck_assert_uint_ge (lm->gc.allocations_since_collection,
                     lm->gc.allocation_budget);
  ck_assert_uint_eq (lm->gc.collections, collections);
  ck_assert (!pool_gc_is_free (garbage));

  EvaluationResult result = lm_evaluate (lm, forms);
  ck_assert (result.success);
  ck_assert_uint_eq (lm->gc.collections, collections + 1);
  ck_assert (pool_gc_is_free (garbage));
  ck_assert_uint_eq (length (result.value), count);
  for (Cell *p = result.value; cell_is_pair (p); p = CDR (p))
    {
      ck_assert (cell_has_type (CAR (p), CELL_TYPE_INTEGER));
      ck_assert_int_eq (CAR (p)->integer, 1);
    }
}
END_TEST

START_TEST (test_budget_adapts_to_live_cells_and_reserved_capacity)
{
  size_t baseline_live   = lm->gc.live_cells;
  size_t baseline_budget = lm->gc.allocation_budget;
  size_t count           = baseline_budget * 2 + 17;
  Cell  *live            = NIL;
  for (size_t i = 0; i < count; ++i)
    live = cell_make_pair (lm, T, live);
  lm->stk.cells[lm->stk.sp++] = live;
  lm_gc (lm);
  ck_assert_uint_eq (lm->gc.live_cells, baseline_live + count);
  ck_assert_uint_ge (lm->gc.allocation_budget, lm->gc.live_cells);
  ck_assert_uint_gt (lm->gc.allocation_budget, baseline_budget);
  ck_assert_uint_eq (lm->gc.allocations_since_collection, 0);

  --lm->stk.sp;
  lm_gc (lm);
  ck_assert_uint_eq (lm->gc.live_cells, baseline_live);
  size_t capacity = 0;
  for (Pool *pool = lm->pool; pool; pool = pool->next)
    capacity += pool->count;
  // Pools remain reserved even after a spike. Avoid rescanning their empty
  // slots after only a tiny number of allocations on the now-small live heap.
  ck_assert_uint_ge (lm->gc.allocation_budget, capacity / 2);
  ck_assert_uint_ge (lm->gc.allocation_budget, LM_GC_MIN_ALLOCATIONS);
}
END_TEST

Suite *
gc_suite (void)
{
  Suite *suite = suite_create ("GC Scheduling");
  TCase *tc    = tcase_create ("Allocation pressure");
  tcase_add_checked_fixture (tc, setup, teardown);
  tcase_add_test (tc, test_nonallocating_steps_do_not_collect);
  tcase_add_test (tc, test_pressure_accumulates_across_evaluations);
  tcase_add_test (tc, test_parser_pressure_waits_for_rooted_evaluation);
  tcase_add_test (tc, test_budget_adapts_to_live_cells_and_reserved_capacity);
  suite_add_tcase (suite, tc);
  return suite;
}
