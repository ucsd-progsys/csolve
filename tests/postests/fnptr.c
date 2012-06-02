#include <csolve.h>

int foo(int *p) {return *p;}

int main()
{
  int x = 3;
  int (*fptr)(int *) = foo;

  fptr(&x);

  return 0;
}
