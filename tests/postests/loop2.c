#include <csolve.h>

void main(){
  int i, j; 
 
  i = 0;
  j = 0;
  
  while (nondet()){
    i++;
    j++;
  }

  csolve_assert(i==j);
 
}
