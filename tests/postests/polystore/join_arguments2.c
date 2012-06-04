#include <csolve.h>
#include <stdlib.h>

extern
void fun(void * VAR(x) a)
  OKEXTERN;

void main(void)
{
  int x;
  char a[2];
  void *ptr;
  void *z;
  if(nondet()) {
    ptr = &x;
  } else {
    ptr = &a[1];
  }
  fun(ptr);
}
