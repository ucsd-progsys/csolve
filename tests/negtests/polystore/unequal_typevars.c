#include <csolve.h>

void test(void * LOC(L) x, void * LOC(L) y)
{
  csolve_assert(x == y);
}
