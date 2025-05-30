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

START_TEST (test_copy)
{
  List *lst_cpy = NULL;
  int v[3] = { 1, 2, 3 };

  // empty
  lst_cpy = list_copy (lst);
  ck_assert_int_eq (lst_cpy->count, 0);
  list_destroy (lst_cpy);

  list_append (lst, &v[0]);
  lst_cpy = list_copy (lst);
  ck_assert_int_eq (lst_cpy->count, 1);
  ck_assert_ptr_eq (lst_cpy->items[0], &v[0]);
  list_destroy (lst_cpy);

  list_append (lst, &v[1]);
  lst_cpy = list_copy (lst);
  ck_assert_int_eq (lst_cpy->count, 2);
  ck_assert_ptr_eq (lst_cpy->items[1], &v[1]);
  list_destroy (lst_cpy);

  list_append (lst, &v[2]);
  lst_cpy = list_copy (lst);
  ck_assert_int_eq (lst_cpy->count, 3);
  ck_assert_ptr_eq (lst_cpy->items[2], &v[2]);
  list_destroy (lst_cpy);
}
END_TEST

START_TEST (test_reverse)
{
  // empty list
  list_reverse (lst);
  ck_assert_int_eq (lst->count, 0);

  // one item
  List *lst1 = list_create ();
  int v1[1] = { 1 };
  list_append (lst1, &v1[0]);
  list_reverse (lst1);
  ck_assert_int_eq (lst1->count, 1);
  ck_assert_ptr_eq (lst1->items[0], &v1[0]);
  list_destroy (lst1);

  // two items (even)
  List *lst2 = list_create ();
  int v2[2] = { 1, 2 };
  list_append (lst2, &v2[0]);
  list_append (lst2, &v2[1]);
  list_reverse (lst2);
  ck_assert_int_eq (lst2->count, 2);
  ck_assert_ptr_eq (lst2->items[0], &v2[1]);
  ck_assert_ptr_eq (lst2->items[1], &v2[0]);
  list_destroy (lst2);

  // two three (odd)
  List *lst3 = list_create ();
  int v3[3] = { 1, 2, 3 };
  list_append (lst3, &v3[0]);
  list_append (lst3, &v3[1]);
  list_append (lst3, &v3[2]);
  list_reverse (lst3);
  ck_assert_int_eq (lst3->count, 3);
  ck_assert_ptr_eq (lst3->items[0], &v3[2]);
  ck_assert_ptr_eq (lst3->items[1], &v3[1]);
  ck_assert_ptr_eq (lst3->items[2], &v3[0]);
  list_destroy (lst3);
}
END_TEST

Suite *
list_suite (void)
{
  Suite *s = suite_create ("List");

  TCase *tc = tcase_create ("Core");
  tcase_add_checked_fixture (tc, setup, teardown);
  tcase_add_test (tc, test_append_and_count);
  tcase_add_test (tc, test_capacity_growth);
  tcase_add_test (tc, test_remove_index_middle);
  tcase_add_test (tc, test_remove_index_edges);
  tcase_add_test (tc, test_copy);
  tcase_add_test (tc, test_reverse);
  suite_add_tcase (s, tc);

  return s;
}
