#include <cpj.h>
#include <stdlib.h>


void main(){
  int i;
  int n = nondetpos();
 
  foreach(i, 0, n)
    csolve_assert((0 <= i));
    csolve_assert((i <  n));
  endfor 
  
  //int tmp = csolve_assume((n > 10));   // SAFE
  //csolve_assert(n > 10);


}
