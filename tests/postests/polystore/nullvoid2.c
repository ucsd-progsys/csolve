//! run with --notruekvars
#include <csolve.h>

void foo(void *REF(true) *p)
{
  *p = 0;
}
