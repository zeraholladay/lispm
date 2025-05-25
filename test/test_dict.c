#include <check.h>
#include <uuid/uuid.h>

#include "dict.h"

#define N_FUZZ 1000

Dict *dict;

void
setup (void)
{
  dict = dict_alloc (NULL, 0);
  ck_assert_ptr_nonnull (dict);
}

void
teardown (void)
{
  dict_destroy (dict);
}

static const char *keys[] = { "alpha", "beta", "gamma", "delta" };
static int vals_int[] = { 10, 20, 30, 40 };

START_TEST (test_insert_and_lookup)
{
  DictEntity *entity;

  ck_assert (dict_insert (dict, "foo", (void *)(intptr_t)42) >= 0);
  entity = dict_lookup (dict, "foo");
  ck_assert_ptr_nonnull (entity);
  ck_assert_int_eq ((intptr_t)entity->val, 42);
  ck_assert (strlen ("foo") == entity->len);

  // missing key
  ck_assert_ptr_null (dict_lookup (dict, "bar"));
}
END_TEST

START_TEST (test_insert_override)
{
  DictEntity *entity;

  ck_assert (dict_insert (dict, "foo", (void *)(intptr_t)42) >= 0);
  entity = dict_lookup (dict, "foo");
  ck_assert_ptr_nonnull (entity);
  ck_assert_int_eq ((intptr_t)entity->val, 42);

  // Insert same key with new value
  ck_assert (dict_insert (dict, "foo", (void *)(intptr_t)7) >= 0);
  entity = dict_lookup (dict, "foo");
  ck_assert_ptr_nonnull (entity);
  ck_assert_int_eq ((intptr_t)entity->val, 7);
}
END_TEST

START_TEST (test_delete_existing)
{
  DictEntity *entity;

  ck_assert (dict_insert (dict, "foo", (void *)(intptr_t)42) >= 0);
  entity = dict_lookup (dict, "foo");
  ck_assert_ptr_nonnull (entity);
  ck_assert_int_eq ((intptr_t)entity->val, 42);

  size_t old_count = dict->count;

  dict_del (dict, "foo");
  ck_assert_ptr_null (dict_lookup (dict, "foo"));
  ck_assert_int_eq (dict->count, old_count - 1);
}
END_TEST

START_TEST (test_delete_nonexistent)
{
  DictEntity *entity;

  // Deleting a missing key should be safe
  dict_del (dict, "foo");
  // Still returns not found
  entity = dict_lookup (dict, "foo");
  ck_assert_ptr_null (entity);
}
END_TEST

START_TEST (test_multiple_entries)
{
  DictEntity *entity;

  for (size_t i = 0; i < 4; i++)
    {
      ck_assert (dict_insert (dict, keys[i], &vals_int[i]) >= 0);
    }

  for (size_t i = 0; i < 4; i++)
    {
      entity = dict_lookup (dict, keys[i]);
      ck_assert_ptr_nonnull (entity);
      ck_assert_ptr_eq (entity->val, &vals_int[i]);
    }

  ck_assert_ptr_null (dict_lookup (dict, "epsilon"));
}
END_TEST

START_TEST (test_initialization)
{
  static DictEntity entities[]
      = { DICT_ENTITY ("alpha", (void *)(intptr_t)42),
          DICT_ENTITY ("beta", (void *)(intptr_t)100),
          DICT_ENTITY ("gamma", (void *)(intptr_t)-7),
          DICT_ENTITY ("delta", (void *)"hello"),
          DICT_ENTITY ("epsilon", (void *)(intptr_t)99999) };
  Dict *local_dict;
  DictEntity *entity;

  local_dict = dict_alloc (entities, 5);
  ck_assert_ptr_nonnull (local_dict);

  entity = dict_lookup (local_dict, "delta");
  ck_assert_ptr_nonnull (entity);
  ck_assert_str_eq (entity->val, "hello");

  dict_destroy (local_dict);
}

START_TEST (test_initialization_va_list)
{
  DictEntity *entity;

  Dict *local_dict
      = dict_alloc_va_list ("foo", (intptr_t)-42, "bar", (intptr_t)42, NULL);
  ck_assert_ptr_nonnull (local_dict);

  entity = dict_lookup (local_dict, "bar");
  ck_assert_ptr_nonnull (entity);
  ck_assert_int_eq ((intptr_t)entity->val, 42);

  entity = dict_lookup (local_dict, "foo");
  ck_assert_ptr_nonnull (entity);
  ck_assert_int_eq ((intptr_t)entity->val, -42);

  dict_destroy (local_dict);
}

START_TEST (test_uuid_fuzz)
{
  struct
  {
    char key[37];
    int val;
  } uuid_fuzz[N_FUZZ];
  Dict *local_dict = dict_alloc (NULL, 0);
  DictEntity *entity;
  uuid_t uuid;

  ck_assert_ptr_nonnull (local_dict);

  for (int i = 0; i < N_FUZZ; ++i)
    {
      uuid_generate (uuid);
      uuid_unparse_lower (uuid, uuid_fuzz[i].key);
      uuid_fuzz[i].val = i;
      ck_assert (
          dict_insert (local_dict, uuid_fuzz[i].key, (void *)(intptr_t)i)
          >= 0);
    }

  for (int i = 0; i < N_FUZZ; ++i)
    {
      char *key = uuid_fuzz[i].key;
      entity = dict_lookup (local_dict, key);
      ck_assert_ptr_nonnull (entity);
      ck_assert_int_eq ((intptr_t)entity->val, i);
    }

  ck_assert_int_eq (local_dict->count, N_FUZZ);

  // delete every 7th
  for (int i = 0; i < N_FUZZ; ++i)
    {
      char *key = uuid_fuzz[i].key;
      size_t old_count = local_dict->count;

      if (i % 7 == 0)
        {
          dict_del (local_dict, key);
          entity = dict_lookup (local_dict, key);
          ck_assert_ptr_null (entity);
          ck_assert_int_eq (local_dict->count, old_count - 1);
        }
      else
        {
          entity = dict_lookup (local_dict, key);
          ck_assert_ptr_nonnull (entity);
          ck_assert_int_eq ((intptr_t)entity->val, i);
          ck_assert_int_eq (local_dict->count, old_count);
        }
    }

  // delete every 7th by 3 for some randomness
  for (int i = 0; i < N_FUZZ; i += 3)
    {
      char *key = uuid_fuzz[i].key;
      size_t old_count = local_dict->count;

      if (i % 7 == 0)
        {
          entity = dict_lookup (local_dict, key);
          ck_assert_ptr_null (entity);
          ck_assert_int_eq (local_dict->count, old_count);
        }
      else
        {
          entity = dict_lookup (local_dict, key);
          ck_assert_ptr_nonnull (entity);
          ck_assert_int_eq ((intptr_t)entity->val, i);
          ck_assert_int_eq (local_dict->count, old_count);
        }
    }

  dict_destroy (local_dict);
}

Suite *
dict_suite (void)
{
  Suite *s = suite_create ("Dict");
  TCase *tc = tcase_create ("Core");
  tcase_add_checked_fixture (tc, setup, teardown);

  tcase_add_test (tc, test_insert_and_lookup);
  tcase_add_test (tc, test_insert_override);
  tcase_add_test (tc, test_delete_existing);
  tcase_add_test (tc, test_delete_nonexistent);
  tcase_add_test (tc, test_multiple_entries);
  tcase_add_test (tc, test_initialization);
  tcase_add_test (tc, test_initialization_va_list);
  tcase_add_test (tc, test_uuid_fuzz);

  suite_add_tcase (s, tc);
  return s;
}
