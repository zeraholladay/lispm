#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "str_intern.h"

static const char *test_str_intern_similar_strings[]
    = { "example",   "exomple",  "exampl",    "exampel",  "exampqle",
        "exmple",    "exaple",   "examnple",  "exmaple",  "examplex",
        "exapmle",   "examplor", "exampole",  "xample",   "examplely",
        "exambple",  "examp",    "exampler",  "exampyle", "examplish",
        "examplerz", "exumple",  "exanpl",    "exampxel", "exampgle",
        "eximle",    "exadle",   "exanomple", "exnaple",  "examplyx",
        "exampole",  "examplot", "exumpole",  "zample",   "exemplar",
        "exawple",   "eximpa",   "examplar",  "eximpyle", "example" };

#define NUM_STRINGS                                                           \
  ((int)(sizeof (test_str_intern_similar_strings)                             \
         / sizeof (test_str_intern_similar_strings[0])))

START_TEST (test_str_intern)
{
  struct
  {
    size_t      len;
    const char *saved_str;
  } saved[NUM_STRINGS];

  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      saved[i].len = strlen (test_str_intern_similar_strings[i]);
      saved[i].saved_str
          = str_intern (test_str_intern_similar_strings[i], saved[i].len);
      ck_assert_str_eq (saved[i].saved_str,
                        test_str_intern_similar_strings[i]);
    }

  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      const char *saved_str
          = str_intern (test_str_intern_similar_strings[i], saved[i].len);
      ck_assert_str_eq (saved[i].saved_str, saved_str);
      ck_assert (saved[i].len == strlen (test_str_intern_similar_strings[i]));
    }

  for (int i = 0; i < NUM_STRINGS; ++i)
    {
      for (int j = 0; j < NUM_STRINGS; ++j)
        {
          const char *saved_str_i
              = str_intern (test_str_intern_similar_strings[i],
                            strlen (test_str_intern_similar_strings[i]));
          const char *saved_str_j
              = str_intern (test_str_intern_similar_strings[j],
                            strlen (test_str_intern_similar_strings[j]));
          if (strcmp (test_str_intern_similar_strings[i],
                      test_str_intern_similar_strings[j])
              == 0)
            ck_assert_str_eq (saved_str_i, saved_str_j);
          else
            {
              ck_assert_msg (strcmp (saved_str_i, saved_str_j),
                             "Assertion failed: strings equal (\"%s\" == "
                             "\"%s\") for %s and %s",
                             saved_str_i, saved_str_j,
                             test_str_intern_similar_strings[i],
                             test_str_intern_similar_strings[j]);
            }
        }
    }
}
END_TEST

START_TEST (test_intern_boundaries)
{
  char        slice[] = { 'a', 'b', 'c' };
  const char *saved   = str_intern (slice, sizeof (slice));
  ck_assert_str_eq (saved, "abc");
  ck_assert_ptr_eq (saved, str_intern ("abc", 3));

  size_t len   = 8192;
  char  *large = malloc (len + 1);
  if (!large)
    {
      ck_abort_msg ("Unable to allocate the large-symbol test buffer");
      return;
    }
  memset (large, 'z', len);
  large[len] = '\0';
  saved      = str_intern (large, len);
  ck_assert_str_eq (saved, large);
  ck_assert_ptr_eq (saved, str_intern (large, len));
  free (large);

  for (size_t i = 0; i < 4106; ++i)
    {
      char key[64];
      snprintf (key, sizeof (key), "unique_%zu", i);
      const char *value = str_intern (key, strlen (key));
      ck_assert_str_eq (value, key);
      ck_assert_ptr_eq (value, str_intern (key, strlen (key)));
    }
  ck_assert_ptr_eq (saved, str_intern (saved, len));
  str_intern_destroy ();
  ck_assert_str_eq (str_intern ("reset", 5), "reset");
}
END_TEST

START_TEST (test_distinct_byte_slices)
{
  char        first[]  = { 'a', '\0', 'b' };
  char        second[] = { 'a', '\0', 'c' };
  const char *a        = str_intern (first, sizeof (first));
  const char *b        = str_intern (second, sizeof (second));
  ck_assert_ptr_ne (a, b);
  ck_assert (memcmp (a, first, sizeof (first)) == 0);
  ck_assert_ptr_eq (a, str_intern (first, sizeof (first)));
  ck_assert_ptr_ne (a, str_intern ("a", 1));
  ck_assert_ptr_eq (str_intern ("", 0), str_intern ("", 0));
}
END_TEST

Suite *
str_save_suite (void)
{
  Suite *s       = suite_create ("String Intern");
  TCase *tc_core = tcase_create ("Core");
  tcase_add_test (tc_core, test_str_intern);
  tcase_add_test (tc_core, test_intern_boundaries);
  tcase_add_test (tc_core, test_distinct_byte_slices);
  suite_add_tcase (s, tc_core);
  return s;
}
