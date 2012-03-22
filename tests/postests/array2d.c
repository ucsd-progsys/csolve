#include <csolve.h>
#include <stdlib.h>

int main(){
  
  int dimx = nondetpos();
  int dimy = nondetpos();
  
  int size = csolve_times(dimx, dimy) * sizeof(char);

  csolve_assert(0 < size);

  char * ghost0    = (char *)  malloc(size);
  
  char * ARRAY * ARRAY a = (char **) malloc(dimx * sizeof(char *));

  char * ghost = ghost0;
  
  int i = 0;
  CSOLVE_ASSUME (0 <= (i * dimy));
  CSOLVE_ASSUME (ghost == ghost0 + (i * dimy));
 
  csolve_assert (ghost <= csolve_block_end(ghost0));
 
  for (int i = 0; i < dimx; i++){
    int dummy = csolve_times(i, dimy);
    int dummy = csolve_axiom_times_mono(i+1, dimx, dimy);

    //csolve_assert(ghost == ghost0 + (i * dimy)); 
    //csolve_assert(i < dimx);
    //csolve_assert((i * dimy) + dimy <= (dimx * dimy));
    
    csolve_assert(csolve_block_begin(ghost) <= ghost);
    csolve_assert((ghost + dimy) <= (csolve_block_end(ghost)));
    
    a[i] = ghost;
    ghost += dimy;
  }

  for (int i = 0; i < dimx; i++){
    for (int j = 0; j < dimy; j++){
      csolve_assert(j < dimy);
      //a[i][j] = '0';
    }
  }



  return 0;
}
