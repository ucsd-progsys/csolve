#include <csolve.h>

char buf[8];
int foo(int REF(&& [V >= 0; V < 8]) i)
{
  return buf[i];
}

int bar()
{
  return 0;
}

int main()
{
  int (*fptr)(int REF(&& [V >= 0; V < 8])) = foo;
  /* if (nondet()) */
  /*   fptr = bar; */
  fptr(7);
  //  foo(7);
}
