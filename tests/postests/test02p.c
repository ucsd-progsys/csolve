#include <stdlib.h>

int main(){
  int *r;
  int y;

  r = (int*) malloc(4);

  if (nondet()){ *r = -10; }

  *r = 0;
  while (nondet()){
   *r = *r + 1;
  }

  y = *r;
  csolve_assert(y >= 0);
  return 0;
}
