#include <stdlib.h>

int main(){
  int *r;
  int z;

  r = (int*) malloc(4);

  *r = 0;
  csolve_assert(*r == 0);

  if (nondet()){ 
    z = 0;
  }

  *r = 1;
  csolve_assert(*r == 1);

  return 0;
}
