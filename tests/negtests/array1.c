#include <stdlib.h>
#include <liquidc.h>

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
