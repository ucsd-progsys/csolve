#include <csolve.h>
#include <stdlib.h>

int main(){
  
  int xdim = nondetpos();
  int ydim = nondetpos();

  int * ARRAY * ARRAY a = (int **) malloc(xdim * sizeof(int*));
  int * ghost0    = (int *)  malloc(xdim * ydim * sizeof(int));

  

  //int * ghost = ghost0;
  //int dummy = 0;
  //CSOLVE_ASSUME((dummy * xdim) == 0);
  
  int dummy = nondetpos();
  
  int *ghost = ghost0 + (dummy * xdim) ;
  
  csolve_assert(ghost == ghost0 + (dummy * xdim)); 

  CSOLVE_ASSUME (((dummy * xdim) + xdim) == ((dummy + 1) * xdim));
  
  dummy += 1;
  ghost += xdim;
  
  csolve_assert(ghost == ghost0 + (dummy * xdim)); 
   

  //for (int i = 0; i < xdim; i++){
  //  CSOLVE_ASSUME (((i * xdim) + xdim) == ((i + 1) * xdim));
  //  csolve_assert(ghost == (ghost0 + (i * xdim))); 
  //  
  //  //a[i] = ghost;
  //  ghost += xdim;
  //}

  //for (int i = 0; i < xdim; i++){
  //  for (int j = 0; j < ydim; j++){
  //    a[i][j] = 0;
  //  }
  //}
 

  return 0;
}
