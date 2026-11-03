#ifndef LM_GC_H
#define LM_GC_H

#include "lm.h"

// Collect at a point where all live Cells are rooted in VM
// stacks/environments. Resets allocation pressure and adapts the next budget
// to the surviving heap and reserved pool capacity. The VM calls this between
// completed steps.
void lm_gc (LM *lm);

#endif
