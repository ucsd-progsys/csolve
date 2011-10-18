#include <cpj.h>
#include <stdlib.h>


void main(){
  int i;
  int n = nondetpos();
 
  foreach(i, 0, n)
    lcc_assert((0 <= i));
    lcc_assert((i <  n));
  endfor 
  
  //int tmp = lcc_assume((n > 10));   // SAFE
  //lcc_assert(n > 10);


}
