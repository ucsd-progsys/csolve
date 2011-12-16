

#include <stdlib.h>

extern void foo(int x, int REF(V > x) *y) OKEXTERN;

void main(){
 
  int z;

  for(int i = 0; i < 100; i++){
    int z;
    //z = i + 1;
    foo(i, &z);
    csolve_assert(0);
  }

}



