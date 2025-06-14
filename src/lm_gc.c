#include "lm_gc.h"
#include "lm_secd.h"

static void
gc_mark (LM *lm)
{
  for (size_t i = 0; i < lm->stk.sp; ++i)
    {
    }
  for (size_t i = 0; i < lm->stk.sp; ++i)
    {
    }
  for (size_t i = 0; i < lm->ctl.sp; ++i)
    {
    }
}

static void
gc_sweep (LM *lm)
{
  (void)lm;
}

void
lm_gc (LM *lm)
{
  gc_mark (lm);
  gc_sweep (lm);
}
