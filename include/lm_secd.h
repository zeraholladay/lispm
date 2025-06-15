#ifndef LM_SECD_H
#define LM_SECD_H

#include <stdbool.h>
#include <stddef.h>

#include "dict.h"
#include "palloc.h"
#include "types.h"

#ifndef LISPM_STK_MAX
#define LISPM_STK_MAX 1024
#endif

#ifndef LISPM_ENV_MAX
#define LISPM_ENV_MAX 512
#endif

#ifndef LISPM_CTL_MAX
#define LISPM_CTL_MAX 512
#endif

#ifndef LISPM_DUMP_MAX
#define LISPM_DUMP_MAX 512
#endif

#ifndef LM_OBJ_POOL_CAP
#define LM_OBJ_POOL_CAP 1024
#endif

#define LM_ERR_STATE(_label)                                                  \
  _label:                                                                     \
  fputs ("*** " #_label ":", stderr);                                         \
  goto reset;

#define LM_ERR_HANDLERS(lm, ...)                                              \
  do                                                                          \
    {                                                                         \
      __VA_ARGS__                                                             \
    reset:                                                                    \
      lm_reset (lm);                                                          \
      return NIL;                                                             \
    }                                                                         \
  while (0)

typedef enum
{
#define X(tag, ...) s_##tag,
#include "lm.def"
#undef X
  COUNT,
} StateEnum;

typedef union
{
#define X(tag, ...)                                                           \
  struct                                                                      \
  {                                                                           \
    __VA_ARGS__;                                                              \
  } tag;
#include "lm.def"
#undef X
} Union;

typedef struct state
{
  StateEnum s;
  Union u;
} State;

typedef struct dump
{
  size_t stk_sp, env_sp, ctl_sp;
} Dump;

typedef struct lispm_secd
{
  struct
  {
    size_t sp;
    Cell *cells[LISPM_STK_MAX];
  } stk;
  struct lm_secd
  {
    size_t sp;
    Dict *dict[LISPM_ENV_MAX];
  } env;
  struct
  {
    size_t sp;
    State states[LISPM_CTL_MAX];
  } ctl;
  struct
  {
    size_t sp;
    Dump dumps[LISPM_DUMP_MAX];
  } dmp;
  Pool *pool;
  bool err_bool;
} LM;

#endif
