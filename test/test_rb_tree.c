#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "rb_tree.h"
#include "safe_str.h"

rb_node *
make_node (const char *key, size_t len)
{
  rb_node *n = rb_xalloc ();

  RB_KEY (n) = safe_strndup (key, len);
  RB_KEY_LEN (n) = len;

  return n;
}

START_TEST (test_insert_and_lookup)
{
  rb_node *root = NULL;

  char *apple = "apple";
  char *cherry = "cherry";
  char *banana = "banana";

  rb_node *n1 = make_node (apple, sizeof (apple));
  rb_node *n2 = make_node (banana, sizeof (banana));
  rb_node *n3 = make_node (cherry, sizeof (cherry));

  rb_insert (&root, n1);
  rb_insert (&root, n2);
  rb_insert (&root, n3);

  rb_node *result;

  result = rb_lookup (root, apple, sizeof (apple) - 1);
  ck_assert_ptr_nonnull (result);
  ck_assert_str_eq (result->key, apple);

  result = rb_lookup (root, banana, sizeof (banana) - 1);
  ck_assert_ptr_nonnull (result);
  ck_assert_str_eq (result->key, banana);

  result = rb_lookup (root, cherry, sizeof (cherry) - 1);
  ck_assert_ptr_nonnull (result);
  ck_assert_str_eq (result->key, cherry);

  result = rb_lookup (root, "date", sizeof ("date") - 1);
  ck_assert_ptr_null (result);

  free ((void *)n1->key);
  free ((void *)n2->key);
  free ((void *)n3->key);
  free (n1);
  free (n2);
  free (n3);
}
END_TEST

START_TEST (test_remove)
{
  rb_node *root = NULL;

  char *apple = "apple";
  char *cherry = "cherry";
  char *banana = "banana";

  rb_insert (&root, make_node (apple, sizeof (apple)));
  rb_insert (&root, make_node (banana, sizeof (banana)));
  rb_insert (&root, make_node (cherry, sizeof (cherry)));

  // Test removal
  rb_node *found_b = rb_lookup (root, banana, sizeof (banana) - 1);
  ck_assert_ptr_nonnull (found_b);
  ck_assert_str_eq (found_b->key, banana);

  const char *removed_key
      = found_b->key; // ie removed may be a different node.
  rb_node *removed = rb_remove (&root, found_b);

  ck_assert_ptr_nonnull (removed);
  ck_assert_str_eq (removed_key, banana);
  ck_assert_ptr_null (rb_lookup (root, banana, sizeof (banana) - 1));

  free ((void *)removed_key);
  free (removed);

  // Verify the rest and cleanup
  rb_node *found_apple = rb_lookup (root, apple, sizeof (apple) - 1);
  ck_assert_ptr_nonnull (found_apple);
  ck_assert_str_eq (found_apple->key, apple);

  rb_node *found_cherry = rb_lookup (root, cherry, sizeof (cherry) - 1);
  ck_assert_ptr_nonnull (found_cherry);
  ck_assert_str_eq (found_cherry->key, cherry);

  free ((void *)found_apple->key);
  free ((void *)found_cherry->key);
  free (found_apple);
  free (found_cherry);
}
END_TEST

Suite *
rb_tree_suite (void)
{
  Suite *s;
  TCase *tc_core;

  s = suite_create ("RedBlackTree");
  tc_core = tcase_create ("Core");

  tcase_add_test (tc_core, test_insert_and_lookup);
  tcase_add_test (tc_core, test_remove);
  suite_add_tcase (s, tc_core);

  return s;
}
