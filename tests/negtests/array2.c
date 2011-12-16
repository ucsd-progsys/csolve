#include <stdlib.h>
#include <csolve.h>

void main(){
  int *x;
  int i;

  x = (int *) malloc(100 * sizeof(int));
  
  i = 0;
  
  for (; i < 101; i++){  
    validptr(x);
    *x = 0;
    x++;
  }
  
  return 0;
}
