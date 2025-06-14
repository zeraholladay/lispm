#include <check.h>
#include <stdlib.h>

#if YYDEBUG
extern int yydebug;
#endif

#ifdef TEST_MAIN

extern Suite *str_safe_str_suite ();
extern Suite *stack_suite (void);
extern Suite *rb_tree_suite (void);
extern Suite *list_suite (void);
extern Suite *dict_suite (void);
extern Suite *str_save_suite (void);
extern Suite *palloc_suite (void);
extern Suite *env_suite (void);
extern Suite *eval_suite (void);
extern Suite *eval_math_suite (void);
extern Suite *eval_bool_suite (void);

int
main (void)
{
#if YYDEBUG
  yydebug = 1;
#endif

  SRunner *sr = srunner_create (stack_suite ());

  srunner_add_suite (sr, str_safe_str_suite ());
  srunner_add_suite (sr, stack_suite ());
  srunner_add_suite (sr, rb_tree_suite ());
  srunner_add_suite (sr, list_suite ());
  srunner_add_suite (sr, dict_suite ());
  srunner_add_suite (sr, str_save_suite ());
  srunner_add_suite (sr, palloc_suite ());
  srunner_add_suite (sr, eval_suite ());
  srunner_add_suite (sr, eval_math_suite ());
  srunner_add_suite (sr, eval_bool_suite ());

  srunner_run_all (sr, CK_NORMAL);
  int failed = srunner_ntests_failed (sr);
  srunner_free (sr);

  return (failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}

#endif