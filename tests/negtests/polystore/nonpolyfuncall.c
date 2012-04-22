#include <csolve.h>
#include <stdlib.h>

extern int* LOC(L) bar(int* REF(true) LOC(L) x) OKEXTERN;

void foo(void * VAR(a) x) CHECK_TYPE
{
  x = bar(x);
}
