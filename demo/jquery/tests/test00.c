#include <stdlib.h>

void main(){
  int x;
  int a;
  int b;

  x = nondet();


  if (x < 0) {
    x = -x;
  }

  a = (x >= 0);
  
  a = 999;

  csolve_assert(x >= 0);

  csolve_assert(x >= 0);

  return; 
}
