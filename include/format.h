#ifndef __FORMAT_H__
#define __FORMAT_H__

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "types.h"

#define PRINT(node)                                                           \
  do                                                                          \
    {                                                                         \
      char *str = format (node);                                              \
      puts (str);                                                             \
      free (str);                                                             \
    }                                                                         \
  while (0)

char *format (Node *n);

#endif
