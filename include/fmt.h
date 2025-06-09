#ifndef FMT_H
#define FMT_H

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

#define PERROR(cell)                                                          \
  do                                                                          \
    {                                                                         \
      char *str = format (cell);                                              \
      fprintf (stderr, "%s\n", str);                                          \
      free (str);                                                             \
    }                                                                         \
  while (0)

char *format (Cell *n);

#endif
