#include <stdarg.h>
#include <stdio.h>

int
appendf (char *buf, size_t bufcap, size_t *used, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  int n = vsnprintf (buf + *used, bufcap - *used, fmt, ap);
  va_end (ap);
  if (n < 0 || (size_t)n >= bufcap - *used)
    return -1;
  *used += (size_t)n;
  return 0;
}
