#include <csolve.h>

void test(void * VAR(a) x, void * VAR(a) y)
{
  csolve_assert(x == y);
}
