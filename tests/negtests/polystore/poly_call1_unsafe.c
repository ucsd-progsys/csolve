#include <csolve.h>
#include <stdlib.h>

void foo(int * VALIDPTR x) {} 
void qux(void (*fptr)(void *x)) {}

void baz()
{
  qux(foo);
}
