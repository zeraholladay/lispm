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

#define STK_POP(lm)                                                           \
  ({                                                                          \
    if ((lm)->stk.sp == 0)                                                    \
      goto underflow;                                                         \
    (lm)->stk.cells[--(lm)->stk.sp];                                          \
  })

#define STK_PUSH(lm, val)                                                     \
  do                                                                          \
    {                                                                         \
      if ((lm)->stk.sp >= LISPM_STK_MAX)                                      \
        goto overflow;                                                        \
      (lm)->stk.cells[(lm)->stk.sp++] = val;                                  \
    }                                                                         \
  while (0)

#define LM_ENTER_FRAME(lm)                                                    \
  do                                                                          \
    {                                                                         \
      if ((lm)->env.sp >= LISPM_ENV_MAX)                                      \
        goto overflow;                                                        \
      (lm)->env.dict[(lm)->env.sp++] = dict_create (NULL, 0);                 \
    }                                                                         \
  while (0)

#define LM_LEAVE_FRAME(lm)                                                    \
  do                                                                          \
    {                                                                         \
      if ((lm)->env.sp == 0)                                                  \
        goto underflow;                                                       \
      (lm)->env.sp--;                                                         \
      dict_destroy ((lm)->env.dict[(lm)->env.sp]);                            \
    }                                                                         \
  while (0)

#define CTL_POP(lm)                                                           \
  ({                                                                          \
    if ((lm)->ctl.sp == 0)                                                    \
      goto underflow;                                                         \
    (lm)->ctl.states[--(lm)->ctl.sp];                                         \
  })

#define CTL_PUSH(lm, tag, ...)                                                \
  do                                                                          \
    {                                                                         \
      if ((lm)->ctl.sp >= LISPM_CTL_MAX)                                      \
        goto overflow;                                                        \
      (lm)->ctl.states[(lm)->ctl.sp++]                                        \
          = (State){ .s = s_##tag, .u.tag = { __VA_ARGS__ } };                \
    }                                                                         \
  while (0)

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

#define BAIL(lm, err_code, fmt, ...)                                          \
  do                                                                          \
    {                                                                         \
      lm_err_set ((lm), (err_code), (fmt), ##__VA_ARGS__);                    \
      goto error;                                                             \
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
