#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "env.h"
#include "safe_str.h"

#define NUM_STRINGS ((int)(sizeof (test_strings) / sizeof (test_strings[0])))

static Env *frame = NULL;

static const char *test_strings[]
    = { "example",   "exomple",  "exampl",    "exampel",  "exampqle",
        "exmple",    "exaple",   "examnple",  "exmaple",  "examplex",
        "exapmle",   "examplor", "exampole",  "xample",   "examplely",
        "exambple",  "examp",    "exampler",  "exampyle", "examplish",
        "examplerz", "exumple",  "exanpl",    "exampxel", "exampgle",
        "eximle",    "exadle",   "exanomple", "exnaple",  "examplyx",
        "exampole",  "examplot", "exumpole",  "zample",   "exemplar",
        "exawple",   "eximpa",   "examplar",  "eximpyle", "example" };

static void
setup (void)
{
  frame = env_create ();
  ck_assert_ptr_nonnull (frame);

  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      const char *key = test_strings[i];
      void *val = (void *)djb2 (key);

      bool res = env_define (frame, key, val);
      ck_assert (res);
    }
}

static void
teardown (void)
{
  env_destroy (frame);
}

START_TEST (test_env)
{
  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      const char *key = test_strings[i];

      bool has_key = env_has_key (frame, key);
      ck_assert (has_key);

      void *lkup_val = env_lookup (frame, key);
      ck_assert_ptr_nonnull (lkup_val);
      ck_assert_ptr_eq (lkup_val, (void *)djb2 (key));
    }
}
END_TEST

START_TEST (test_env_fail) { ck_assert (!env_has_key (frame, "BOGUS")); }
END_TEST

START_TEST (test_child_override)
{
  env_enter_frame (&frame);

  // child overrides
  const char *key = test_strings[0];
  void *val = (void *)0xDEADBEEF;

  bool res = env_define (frame, key, val);
  ck_assert (res);

  bool has_key = env_has_key (frame, key);
  ck_assert (has_key);

  void *lkup_val = env_lookup (frame, key);
  ck_assert_ptr_nonnull (lkup_val);
  ck_assert_ptr_eq (lkup_val, (void *)val);

  // cleanup
  env_leave_frame (&frame);

  // frame/parent should have a value but not this value
  void *parent_lkup_val = env_lookup (frame, key);
  ck_assert_ptr_nonnull (parent_lkup_val);
  ck_assert_ptr_eq (parent_lkup_val, (void *)djb2 (key));
}
END_TEST

START_TEST (test_set_override)
{
  bool has_key = false;
  void *lkup_val = NULL;

  env_enter_frame (&frame);

  // global but in frame
  const char *key = test_strings[0];
  void *val = (void *)0xDEADBEEF;

  bool res = env_set (frame, key, val);
  ck_assert (res);

  has_key = env_has_key (frame, key);
  ck_assert (!has_key); // key should not exist in this frame

  lkup_val = env_lookup (frame, key);
  ck_assert_ptr_nonnull (lkup_val);
  ck_assert_ptr_eq (lkup_val, (void *)val);

  // cleanup (nothing should have changed)
  env_leave_frame (&frame);

  has_key = env_has_key (frame, key);
  ck_assert (has_key);

  lkup_val = env_lookup (frame, key);
  ck_assert_ptr_nonnull (lkup_val);
  ck_assert_ptr_eq (lkup_val, (void *)val);
}
END_TEST

Suite *
env_suite (void)
{
  Suite *s = suite_create ("Env");
  TCase *tc_core = tcase_create ("Core");
  tcase_add_checked_fixture (tc_core, setup, teardown);
  tcase_add_test (tc_core, test_env);
  tcase_add_test (tc_core, test_env_fail);
  tcase_add_test (tc_core, test_child_override);
  tcase_add_test (tc_core, test_set_override);
  suite_add_tcase (s, tc_core);
  return s;
}
