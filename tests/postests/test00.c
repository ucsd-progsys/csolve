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

  lcc_assert(x >= 0);

  lcc_assert(x >= 0);

  return; 
}
