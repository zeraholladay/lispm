#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "palloc.h"
#include "types.h"

START_TEST (test_palloc)
{
  size_t count = 4096;

  Pool *pool = pool_init (count, sizeof (Cell));

  ck_assert (pool);

  for (size_t i = 0; i < count + 1; ++i)
    {
      ck_assert (pool_xalloc (pool));
    }

  // never reached
  pool_reset_all (pool);

  pool_destroy (&pool);
}
END_TEST

START_TEST (test_palloc_hier)
{
  size_t count = 42;

  Pool *pool = pool_init (count, sizeof (Cell));

  ck_assert (pool);

  for (size_t i = 0; i < 100; ++i)
    {
      for (size_t j = 0; j < count + 1; ++j)
        {
          ck_assert (pool_xalloc_hier (&pool));
        }
    }

  pool_destroy_hier (&pool);
  ck_assert (pool == NULL);
}
END_TEST

Suite *
palloc_suite (void)
{
  Suite *s       = suite_create ("Palloc");
  TCase *tc_core = tcase_create ("Core");
  tcase_add_exit_test (tc_core, test_palloc, 1);
  tcase_add_test (tc_core, test_palloc_hier);
  suite_add_tcase (s, tc_core);
  return s;
}
