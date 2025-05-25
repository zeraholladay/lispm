#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stack.h"

// START_TEST(test_push_pop) {
//   Stack stack = {};
//   STACK_INIT(&stack);
//   void *val_in = (void *)0xDEADBEEF;
//   PUSH(&stack, val_in);
//   ck_assert_ptr_eq(POP(&stack), val_in);
//   stack_free(&stack);
// }
// END_TEST

// START_TEST(test_lifo_order) {
//   Stack stack = {};
//   STACK_INIT(&stack);
//   PUSH(&stack, 1);
//   PUSH(&stack, 2);
//   PUSH(&stack, 3);
//   ck_assert_ptr_eq(POP(&stack), (void *)3);
//   ck_assert_ptr_eq(POP(&stack), (void *)2);
//   ck_assert_ptr_eq(POP(&stack), (void *)1);
//   stack_free(&stack);
// }
// END_TEST

// START_TEST(test_peek) {
//   Stack stack = {};
//   STACK_INIT(&stack);
//   void *val = (void *)0x1234;
//   PUSH(&stack, val);
//   ck_assert_ptr_eq(PEEK(&stack), val);
//   ck_assert_ptr_eq(POP(&stack), val);
//   stack_free(&stack);
// }
// END_TEST

// START_TEST(test_enter_exit_frame) {
//   Stack stack = {};
//   STACK_INIT(&stack);
//   uintptr_t old_fp = stack.fp;
//   PUSH(&stack, 100);
//   ENTER_FRAME(&stack);
//   ck_assert_uint_eq(stack.fp, stack.sp);
//   PUSH(&stack, 200);
//   EXIT_FRAME(&stack);
//   ck_assert_uint_eq(stack.fp, old_fp);
//   ck_assert_ptr_eq(PEEK(&stack), (void *)100);
//   stack_free(&stack);
// }
// END_TEST

// START_TEST(test_grow) {
//   Stack stack = {};
//   STACK_INIT(&stack);
//   uintptr_t init_data_size = stack.data_size;
//   for (uintptr_t i = 0; i <= init_data_size; i++) {
//     PUSH(&stack, i);
//   }
//   ck_assert_uint_eq(2 * init_data_size, stack.data_size);
// }
// END_TEST

Suite *
stack_suite (void)
{
  Suite *s = suite_create ("Stack");

  TCase *tc_core = tcase_create ("Core");

  // tcase_add_test(tc_core, test_push_pop);
  // tcase_add_test(tc_core, test_lifo_order);
  // tcase_add_test(tc_core, test_peek);
  // tcase_add_test(tc_core, test_enter_exit_frame);
  // tcase_add_test(tc_core, test_grow);

  suite_add_tcase (s, tc_core);
  return s;
}
