#ifndef FORMAT_H
#define FORMAT_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "types.h"

#define PRINT(cell)                                                           \
  do                                                                          \
    {                                                                         \
      char *str = format (cell);                                              \
      puts (str);                                                             \
      free (str);                                                             \
    }                                                                         \
  while (0)

char *format (Cell *n);

#endif
