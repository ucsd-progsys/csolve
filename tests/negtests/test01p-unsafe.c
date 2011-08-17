#include <stdlib.h>

void main(){
  int *r;
  int y;

  r = (int*) malloc(4 * sizeof(int));

  *r = 5;

  y = *r;

  assert(y >= 10);

  return;
}
