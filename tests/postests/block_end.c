#include <csolve.h>
#include <stdlib.h>

int main(){
  int size = nondetpos();
  csolve_assert(0 < size);
  char * ghost = (char *)  malloc(size);
  
  csolve_assert (ghost = csolve_block_begin(ghost));
  csolve_assert (ghost <= csolve_block_end(ghost));
  return 0;
}
