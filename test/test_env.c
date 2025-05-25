#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "env.h"
#include "safe_str.h"

static const char *test_env_similar_strings[]
    = { "example",   "exomple",  "exampl",    "exampel",  "exampqle",
        "exmple",    "exaple",   "examnple",  "exmaple",  "examplex",
        "exapmle",   "examplor", "exampole",  "xample",   "examplely",
        "exambple",  "examp",    "exampler",  "exampyle", "examplish",
        "examplerz", "exumple",  "exanpl",    "exampxel", "exampgle",
        "eximle",    "exadle",   "exanomple", "exnaple",  "examplyx",
        "exampole",  "examplot", "exumpole",  "zample",   "exemplar",
        "exawple",   "eximpa",   "examplar",  "eximpyle", "example" };

#define NUM_STRINGS                                                           \
  ((int)(sizeof (test_env_similar_strings)                                    \
         / sizeof (test_env_similar_strings[0])))

START_TEST (test_env)
{
  Env *env = env_new (NULL);

  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      void *hash = (void *)djb2 (test_env_similar_strings[i]);
      ck_assert_int_eq (env_set (env, test_env_similar_strings[i], hash), 0);
    }

  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      unsigned long hash = djb2 (test_env_similar_strings[i]);
      rb_node *n = env_lookup (env, test_env_similar_strings[i]);
      ck_assert_str_eq (RB_KEY (n), test_env_similar_strings[i]);
      ck_assert (RB_KEY_LEN (n) == strlen (test_env_similar_strings[i]));
      ck_assert ((unsigned long)RB_VAL (n) == hash);
    }

  for (int i = NUM_STRINGS - 1; i > 0; --i)
    {
      unsigned long hash = djb2 (test_env_similar_strings[i]);
      rb_node *n = env_lookup (env, test_env_similar_strings[i]);
      ck_assert_str_eq (RB_KEY (n), test_env_similar_strings[i]);
      ck_assert (RB_KEY_LEN (n) == strlen (test_env_similar_strings[i]));
      ck_assert ((unsigned long)RB_VAL (n) == hash);
    }

  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      for (int j = 0; j < NUM_STRINGS; ++j)
        {
          unsigned long hash_i = djb2 (test_env_similar_strings[i]);
          rb_node *n_i = env_lookup (env, test_env_similar_strings[i]);

          unsigned long hash_j = djb2 (test_env_similar_strings[j]);
          rb_node *n_j = env_lookup (env, test_env_similar_strings[j]);

          if (hash_i == hash_j)
            ck_assert_str_eq (RB_KEY (n_i), RB_KEY (n_j));
          else
            {
              ck_assert_msg (strcmp (RB_KEY (n_i), RB_KEY (n_j)),
                             "Assertion failed: strings equal (\"%s\" == "
                             "\"%s\") for %s and %s",
                             RB_KEY (n_i), RB_KEY (n_j),
                             test_env_similar_strings[i],
                             test_env_similar_strings[j]);
            }
        }
    }

  // TODO: CLEANUP
}
END_TEST

START_TEST (test_env_frame)
{
  Env *env_parent = env_new (NULL);
  Env *env_child = env_new (env_parent);

  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      void *hash = (void *)djb2 (test_env_similar_strings[i]);
      ck_assert_int_eq (
          env_set (env_parent, test_env_similar_strings[i], hash), 0);
    }

  // fall through from child to parent
  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      unsigned long hash = djb2 (test_env_similar_strings[i]);
      rb_node *n = env_lookup (env_child, test_env_similar_strings[i]);
      ck_assert_str_eq (RB_KEY (n), test_env_similar_strings[i]);
      ck_assert (RB_KEY_LEN (n) == strlen (test_env_similar_strings[i]));
      ck_assert ((unsigned long)RB_VAL (n) == hash);
    }

  // child overrides
  const char *child_override_key1 = test_env_similar_strings[1];
  const char *child_override_key2 = test_env_similar_strings[NUM_STRINGS - 1];

  char *child_val1 = "foo";
  char *child_val2 = "bar";

  env_set (env_child, child_override_key1, child_val1);
  env_set (env_child, child_override_key2, child_val2);

  rb_node *child_n1 = env_lookup (env_child, child_override_key1);
  rb_node *child_n2 = env_lookup (env_child, child_override_key2);

  ck_assert_str_eq (RB_VAL (child_n1), child_val1);
  ck_assert_str_eq (RB_VAL (child_n2), child_val2);

  // TODO: CLEANUP
}
END_TEST

START_TEST (test_env_fail)
{
  Env *env = env_new (NULL);

  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      void *hash = (void *)djb2 (test_env_similar_strings[i]);
      ck_assert_int_eq (env_set (env, test_env_similar_strings[i], hash), 0);
    }
  const char *key_1 = "foo";
  const char *key_2 = "bar";

  void *val_1 = (void *)djb2 (key_1);
  void *val_2 = (void *)djb2 (key_2);

  env_set (env, key_1, val_1);
  env_set (env, key_2, val_2);

  rb_node *n1 = env_lookup (env, key_1);
  rb_node *n2 = env_lookup (env, key_2);
  rb_node *n3 = env_lookup (env, "BOGUS");

  ck_assert (n1);
  ck_assert (n2);
  ck_assert (n3 == NULL);
}
END_TEST

Suite *
env_suite (void)
{
  Suite *s = suite_create ("Env");

  TCase *tc_core = tcase_create ("Core");

  tcase_add_test (tc_core, test_env);
  tcase_add_test (tc_core, test_env_frame);
  tcase_add_test (tc_core, test_env_fail);

  suite_add_tcase (s, tc_core);
  return s;
}
