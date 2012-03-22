#include <csolve.h>
#include <stdlib.h>

//WORKS
//extern int REF(V > 0) USE_INDEX goo (int x, int y) OKEXTERN;

//DOESNT WORK
extern int REF(V > 0) goo (int x) OKEXTERN;

void main(){
  int dimx = nondetpos();
  int size = goo(dimx);
}
