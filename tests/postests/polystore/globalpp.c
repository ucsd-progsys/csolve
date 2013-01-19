#include <csolve.h>

extern int **y OKEXTERN;

extern void foo(void * VAR(a) *,
                void (*)(void * VAR(a))) OKEXTERN;

void bar(int *y) {}

void main()
{
  foo(y, bar);
}
