#include <csolve.h>


int foo(int z){
  if (z > 0) {
    z++;
  } else {
    z--;
  }
  return z;
}

void main(){

  int a, b;

  a = foo (10);

  b = foo (0 - 10);

  csolve_assert (a > 0);
}
