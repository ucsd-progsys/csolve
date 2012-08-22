#include <csolve.h>
#include <stdlib.h>

int main(){
  int jhala = 1000;
  int *r;
  int y;

  r = (int*) malloc(10 * sizeof(int));
  
  int r1[5] = { 1, 2, 3, 4};

  *r = 5;
  y = *r;

  csolve_assert(y >= 0);
  csolve_assert (r1[3]);

  return 0;
}
