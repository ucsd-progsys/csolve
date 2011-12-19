#include <csolve.h>

void main(){
  int i, j, b; 
 
  j = 0; 
  
  //b = nondet(); j = b;
  
  for(i = 0; i < 10; i++)
    j++;

  csolve_assert(0 <= j);
  csolve_assert(j == 10);
 
  //csolve_assert(b <= j && j <= b + 10);
}
