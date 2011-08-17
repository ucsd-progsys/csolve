#include <stdlib.h>

void main(){
  int *x;
  int *y;

  int i;

  y = (int *) malloc(100 * sizeof(int));
  
  x = y;

  i = 0;
  
  for (; i < 100; i++){  
    validptr(x);
    *x = 0;
    x++;
  }

  return;
}
