#include <csolve.h>
#include <stdlib.h>

//WORKS
//extern int REF(V > 0) USE_INDEX goo (int x, int y) OKEXTERN;

//DOESNT WORK
extern int REF(V > 0) goo (int x, int y) OKEXTERN;

int main(){
  
  int dimx = nondetpos();
  int dimy = nondetpos();

  int size = goo(dimx, dimy);

  return 0;
}
