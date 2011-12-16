#include <stdlib.h>
#include <csolve.h>

void main(){
  int *x;
  int *y;
  int i;
  int z;

  y = (int *) malloc(100 * sizeof(int));
  
  x = y;
  i = 0;
  
  for (; i < 100; i++){  
    validptr(x);
    *x = 0;
    x++;
  }

  i = 0;
  x = y;
  for (; i < 100; i++){
    z = *x;
    csolve_assert(z >= 10);
    x++;
  }
  return 0;
}
