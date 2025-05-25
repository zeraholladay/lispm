#ifndef DEBUG_H
#define DEBUG_H

#include <stdio.h>

#ifdef lispm_DEBUG
#define LISPM_DEBUG_ENABLED 1
#else
#define LISPM_DEBUG_ENABLED 0
#endif

#define DEBUG_STRINGIFY_HELPER(x) #x
#define DEBUG_STRINGIFY(x) DEBUG_STRINGIFY_HELPER (x)
#define DEBUG_LOCATION "[" __FILE__ ":" DEBUG_STRINGIFY (__LINE__) "] "

#ifndef DEBUG
#define DEBUG(msg)                                                            \
  do                                                                          \
    {                                                                         \
      if (LISPM_DEBUG_ENABLED)                                                 \
        fprintf (stderr, "[%s:%d] %s(): %s\n", __FILE__, __LINE__, __func__,  \
                 msg);                                                        \
    }                                                                         \
  while (0)
#endif

#endif
