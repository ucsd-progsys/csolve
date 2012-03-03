#include <csolve.h>
#include <stdlib.h>

extern int* LOC(L) bar(int* LOC(L) x) OKEXTERN;

void
foo(void *x) CHECK_TYPE
{
  x = bar(x);
}
