#include <setjmp.h>
#include <stdio.h>

#include "error.h"

jmp_buf eval_error_jmp;

void
raise (ErrorCode err_code, const char *msg)
{
  const char *err_code_msg = error_messages[err_code];
  fprintf (stderr, "*** eval error: %s: %s\n", err_code_msg, msg);
  longjmp (eval_error_jmp, 1);
}
