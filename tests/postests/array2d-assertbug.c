#include <csolve.h>
#include <stdlib.h>

int main(){
  
  int dimx = nondetpos();
  int dimy = nondetpos();

  int size = csolve_times(dimx, dimy) * sizeof(char);

  csolve_assert(0 < size);

  char * ghost0    = (char *)  malloc(size);
  
  //char * ARRAY * ARRAY a = (char **) malloc(dimx * sizeof(char *));

  char * ghost = ghost0;
  
  int i = 0;

  CSOLVE_ASSUME (0 <= (i * dimy));
  CSOLVE_ASSUME (ghost == ghost0 + (i * dimy));
 
  csolve_assert (ghost <= csolve_block_end(ghost0));
 
  //for (int i = 0; i < dimx; i++){
  //  csolve_assert(ghost == ghost0 + (i * dimy)); 
  //  csolve_assert(i < dimx);
  //  
  //  CSOLVE_ASSUME((i * dimy) + dimy <= (dimx * dimy));
  //  CSOLVE_ASSUME (((i * dimy) + dimy) == ((i + 1) * dimy));
  //  
  //  //UNCOMMENT THIS AND ALL ASSERTS fail ?!
  //  //csolve_assert((ghost + dimy) <= (csolve_block_end(ghost)));
  //  
  //  a[i] = ghost;
  //  ghost += dimy;
  //}

  return 0;
}
