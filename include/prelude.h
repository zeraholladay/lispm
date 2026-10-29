#ifndef PRELUDE_H
#define PRELUDE_H

#include "lm.h"

// Bootstrap the global environment from the Scheme source embedded at build
// time.
bool prelude_load (LM *lm);

#endif
