#include <csolve.h>
#include <stdlib.h>

int main(){
  
  int size = nondetpos();
  
  int * ARRAY a = malloc(size * sizeof(int));
 
  for (int i = 0; i < size; i++){
    a[i] = 100;
  }

  for (int i = 0; i < size; i++){
    int z = a[i];
    csolve_assert(z == 100);
  }



  return 0;
}
