#include <csolve.h>
#include <stdlib.h>

void main(){
  int *ptr;
  int n = nondetpos();
  for(int i = 0; i < n; i++){
    ptr = (int *) malloc(sizeof(int));
  }
  csolve_assert(ptr);
  return;
}
