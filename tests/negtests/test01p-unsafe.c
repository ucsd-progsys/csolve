#include <stdlib.h>

void main(){
  int *r;
  int y;

  r = (int*) malloc(4 * sizeof(int));

  *r = 5;

  y = *r;

  csolve_assert(y >= 10);

  return;
}
