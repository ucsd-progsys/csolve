#include <stdlib.h>
#include <liquidc.h>

void main(){
  int * UNCHECKED t;
  int x;

  t = malloc(0);
  *t = 10;
  //OR DUALLY 
  x = *t; 
  
  return; 
}
