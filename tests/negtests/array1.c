#include <stdlib.h>
#include <csolve.h>

void main(){
  int *x;
  
  x = (int *) malloc(100 * sizeof(int));
  
  while(nondet()){
    validptr(x);
    *x = 0;
    x++;
  }
  
  return;
}
