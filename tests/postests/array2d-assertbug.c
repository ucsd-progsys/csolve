#include <csolve.h>
#include <stdlib.h>

int main(){
  
  int dimx = nondetpos();
  int dimy = nondetpos();

  char * ARRAY * ARRAY a = (char **) malloc(dimx * sizeof(char *));
  char * ghost0    = (char *)  malloc(dimx * dimy * sizeof(char));

  char * ghost = ghost0;
  
  int i = 0;

  CSOLVE_ASSUME (0 <= (i * dimy));
  CSOLVE_ASSUME (ghost == ghost0 + (i * dimy));
 
  for (int i = 0; i < dimx; i++){
    csolve_assert(ghost == ghost0 + (i * dimy)); 
    csolve_assert(i < dimx);
    
    CSOLVE_ASSUME((i * dimy) + dimy <= (dimx * dimy));
    CSOLVE_ASSUME (((i * dimy) + dimy) == ((i + 1) * dimy));
    
    //UNCOMMENT THIS AND ALL ASSERTS fail ?!
    csolve_assert((ghost + dimy) <= (csolve_block_end(ghost)));
    
    a[i] = ghost;
    ghost += dimy;
  }

  return 0;
}
