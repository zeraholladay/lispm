#include <check.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stack.h"

Stack *stack = NULL;

static void
setup (void)
{
  stack = stack_create ();
}

static void
teardown (void)
{
  stack_destroy (stack);
}

START_TEST (test_stack_push_stack_pop)
{
  void *val_in = (void *)0xDEADBEEF;
  stack_push (stack, val_in);
  ck_assert_ptr_eq (stack_pop (stack), val_in);
}
END_TEST

START_TEST (test_lifo_order)
{
  stack_push (stack, (void *)1);
  stack_push (stack, (void *)2);
  stack_push (stack, (void *)3);
  ck_assert_ptr_eq (stack_pop (stack), (void *)3);
  ck_assert_ptr_eq (stack_pop (stack), (void *)2);
  ck_assert_ptr_eq (stack_pop (stack), (void *)1);
}
END_TEST

START_TEST (test_peek)
{
  void *val = (void *)0x1234;
  stack_push (stack, val);
  ck_assert_ptr_eq (stack_peek (stack), val);
  ck_assert_ptr_eq (stack_pop (stack), val);
}
END_TEST

START_TEST (test_enter_exit_frame)
{
  stack_push (stack, (void *)100);
  stack_enter_frame (stack);
  stack_push (stack, (void *)200);
  stack_exit_frame (stack);
  ck_assert_ptr_eq (stack_peek (stack), (void *)100);
}
END_TEST

START_TEST (test_grow)
{
  uintptr_t n = STACK_GROWTH * 2;
  for (uintptr_t i = 0; i <= n; ++i)
    stack_push (stack, (void *)i);
  ck_assert_uint_eq ((uintptr_t)stack_peek (stack), n);
}
END_TEST

Suite *
stack_suite (void)
{
  Suite *s = suite_create ("Stack");
  TCase *tc_core = tcase_create ("Core");
  tcase_add_checked_fixture (tc_core, setup, teardown);
  tcase_add_test (tc_core, test_stack_push_stack_pop);
  tcase_add_test (tc_core, test_lifo_order);
  tcase_add_test (tc_core, test_peek);
  tcase_add_test (tc_core, test_enter_exit_frame);
  tcase_add_test (tc_core, test_grow);
  suite_add_tcase (s, tc_core);
  return s;
}
