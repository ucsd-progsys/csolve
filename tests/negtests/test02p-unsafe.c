#include <stdlib.h>
#include <liquidc.h>

void main(){
  int *r;
  int y;

  r = (int*) malloc(4 * sizeof(int));

  *r = 0;

  while (nondet()){
   *r = *r - 1;
  }

  y = *r;
  assert(y >= 0);
  
  return;
}
