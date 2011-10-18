#include <cpj.h>
#include <stdlib.h>


void main(){
  int i;
  int n = nondetpos();
 
  //lcc_assume((n > 10));             // UNSAFE
  int tmp = lcc_assume((n > 10));   // SAFE
  lcc_assert(n > 10);


}
