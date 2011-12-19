#include <stdlib.h>

int foo(int *x){
  int y;
  //x = x;
  y = *x;
  csolve_assert(y >= 0);
  return 0;
}

int main(){
  int *r;
  int y;

  r = (int*) malloc(4);
  *r = 0;

  while (nondet()){
   *r = *r + 1;
  }
  foo(r);
  
  return 0;
}
