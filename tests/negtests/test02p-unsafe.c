#include <stdlib.h>
#include <csolve.h>

void main(){
  int *r;
  int y;

  r = (int*) malloc(4 * sizeof(int));

  *r = 0;

  while (nondet()){
   *r = *r - 1;
  }

  y = *r;
  csolve_assert(y >= 0);
  
  return;
}
