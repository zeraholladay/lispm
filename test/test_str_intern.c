#include <check.h>
#include <string.h>

#include "rb_tree.h"
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
    size_t len;
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

Suite *
str_save_suite (void)
{
  Suite *s = suite_create ("String Intern");
  TCase *tc_core = tcase_create ("Core");
  tcase_add_test (tc_core, test_str_intern);
  suite_add_tcase (s, tc_core);
  return s;
}
