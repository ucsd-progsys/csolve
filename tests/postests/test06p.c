#include <stdlib.h>
#include <csolve.h>

void main(){
  int * UNCHECKED t;
  int x;

  t = malloc(0);
  *t = 10;
  //OR DUALLY 
  x = *t; 
  
  return; 
}
