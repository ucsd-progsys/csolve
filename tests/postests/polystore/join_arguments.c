#include <csolve.h>
#include <stdlib.h>

extern
void fun(void * VAR(a) a)
  OKEXTERN;

void main(void)
{
  int x;
  char a[2];
  void *ptr;
  if(nondet()) {
    ptr = &x;
  } else {
    ptr = &a[0];
  }
  fun(ptr);
}
