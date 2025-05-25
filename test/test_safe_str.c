#include <check.h>
#include <stdlib.h>
#include <string.h>

#include "safe_str.h"

static const char *test_safe_str_similar_strings[]
    = { "example",   "exomple",  "exampl",    "exampel",  "exampqle",
        "exmple",    "exaple",   "examnple",  "exmaple",  "examplex",
        "exapmle",   "examplor", "exampole",  "xample",   "examplely",
        "exambple",  "examp",    "exampler",  "exampyle", "examplish",
        "examplerz", "exumple",  "exanpl",    "exampxel", "exampgle",
        "eximle",    "exadle",   "exanomple", "exnaple",  "examplyx",
        "exampole",  "examplot", "exumpole",  "zample",   "exemplar",
        "exawple",   "eximpa",   "examplar",  "eximpyle", "example" };

#define NUM_STRINGS                                                           \
  ((int)(sizeof (test_safe_str_similar_strings)                               \
         / sizeof (test_safe_str_similar_strings[0])))

START_TEST (test_safe_strndup)
{
  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      size_t len = strlen (test_safe_str_similar_strings[i]);
      char *dup = safe_strndup (test_safe_str_similar_strings[i], len);
      ck_assert_str_eq (test_safe_str_similar_strings[i], dup);
      free (dup);
    }
}
END_TEST

START_TEST (test_safe_strnlen)
{
  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      size_t len = strlen (test_safe_str_similar_strings[i]);
      ck_assert (safe_strnlen (test_safe_str_similar_strings[i], len)
                 == strlen (test_safe_str_similar_strings[i]));
    }
}
END_TEST

START_TEST (test_safe_strncmp_minlen)
{
  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      size_t len = strlen (test_safe_str_similar_strings[i]);
      ck_assert (safe_strncmp_minlen (test_safe_str_similar_strings[i],
                                      test_safe_str_similar_strings[i],
                                      len + 1)
                 == 0);
    }
}
END_TEST

Suite *
str_safe_str_suite (void)
{
  Suite *s = suite_create ("Safe String");
  TCase *tc_core = tcase_create ("Core");
  tcase_add_test (tc_core, test_safe_strndup);
  tcase_add_test (tc_core, test_safe_strnlen);
  tcase_add_test (tc_core, test_safe_strncmp_minlen);
  suite_add_tcase (s, tc_core);
  return s;
}
