#include <liquidc.h>

void main(){
  int i, j, b; 
 
  j = 0; 
  
  //b = nondet(); j = b;
  
  for(i = 0; i < 10; i++)
    j++;

  lcc_assert(0 <= j);
  lcc_assert(j == 10);
 
  //lcc_assert(b <= j && j <= b + 10);
}
