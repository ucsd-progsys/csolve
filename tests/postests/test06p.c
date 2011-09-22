#include <stdlib.h>
#include <liquidc.h>

void main(){
  int * UNCHECKED t;
  int x;

  t = malloc(0);
  x = *t; 
  //OR DUALLY 
  *t = 10;
  
  return; 
}
