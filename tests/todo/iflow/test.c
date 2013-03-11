#include <csolve.h>

void foo(int * START LOC(L) p1, int * START LOC(L) p2) CHECK_TYPE
{
  csolve_assert(p1 == p2);
}
