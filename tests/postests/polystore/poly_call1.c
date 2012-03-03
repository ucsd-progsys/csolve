#include <csolve.h>
#include <stdlib.h>

void foo(int *xfoo) {} 
void qux(void (*fptr)(void *xfptr)) {}

void baz()
{
  qux(foo);
}
