#include <csolve.h>
#include <stdlib.h>

int main(){
  
  int xdim = nondetpos();
  int ydim = nondetpos();

  int size = xdim * ydim * sizeof(char);

  CSOLVE_ASSUME (size > 1);
  
  char * ARRAY * ARRAY a = (char **) malloc(xdim * sizeof(char *));
  char * ARRAY ghost = (char *) malloc(size);
  
  int i = 0;
  CSOLVE_ASSUME (0 <= (i * xdim));

  csolve_assert (0 <= (i * xdim));

  CSOLVE_ASSUME (ghost == (csolve_block_begin(ghost) + (i * xdim)));

  for (int i = 0; i < xdim; i++){
    csolve_assert(ghost == csolve_block_begin(ghost) + (i * xdim)); 
    CSOLVE_ASSUME (((i * xdim) + xdim) == ((i + 1) * xdim));
    a[i] = ghost;
    ghost += xdim;
  }

  //for (int i = 0; i < xdim; i++){
  //  for (int j = 0; j < ydim; j++){
  //    a[i][j] = '0';
  //  }
  //}

  return 0;
}
