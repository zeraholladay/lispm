#include <check.h>
#include <stdlib.h>

#include "list.h"
#include "xalloc.h"

List *lst = NULL;

static void
setup (void)
{
  lst = list_create ();
  ck_assert_ptr_nonnull (lst);
  ck_assert_int_eq (lst->count, 0);
}

static void
teardown (void)
{
  list_destroy (lst);
}

START_TEST (test_append_and_count)
{
  int a = 1, b = 2;

  ck_assert (list_append (lst, &a));
  ck_assert_int_eq (lst->count, 1);
  ck_assert_ptr_eq (lst->items[0], &a);

  ck_assert (list_append (lst, &b));
  ck_assert_int_eq (lst->count, 2);
  ck_assert_ptr_eq (lst->items[1], &b);
}
END_TEST

START_TEST (test_capacity_growth)
{
  for (int i = 0; i < HEAP_LIST_INIT_CAPACITY + 2; i++)
    {
      int *v = xmalloc (sizeof *v);
      *v = i;
      ck_assert (list_append (lst, v));
      ck_assert_int_eq (lst->count, i + 1);
    }
}
END_TEST

START_TEST (test_remove_index_middle)
{
  int v[3] = { 10, 20, 30 };
  list_append (lst, &v[0]);
  list_append (lst, &v[1]);
  list_append (lst, &v[2]);

  list_remove_index (lst, 1);
  ck_assert_int_eq (lst->count, 2);
  ck_assert_ptr_eq (lst->items[0], &v[0]);
  ck_assert_ptr_eq (lst->items[1], &v[2]);
}
END_TEST

START_TEST (test_remove_index_edges)
{
  int v[2] = { 5, 6 };
  list_append (lst, &v[0]);
  list_append (lst, &v[1]);

  list_remove_index (lst, 0);
  ck_assert_int_eq (lst->count, 1);
  ck_assert_ptr_eq (lst->items[0], &v[1]);

  list_remove_index (lst, 0);
  ck_assert_int_eq (lst->count, 0);

  list_remove_index (lst, 5);
  ck_assert_int_eq (lst->count, 0);
}
END_TEST

Suite *
list_suite (void)
{
  Suite *s = suite_create ("List");

  TCase *tc_core = tcase_create ("Core");
  tcase_add_checked_fixture (tc_core, setup, teardown);
  tcase_add_test (tc_core, test_append_and_count);
  tcase_add_test (tc_core, test_capacity_growth);
  tcase_add_test (tc_core, test_remove_index_middle);
  tcase_add_test (tc_core, test_remove_index_edges);
  suite_add_tcase (s, tc_core);

  return s;
}
