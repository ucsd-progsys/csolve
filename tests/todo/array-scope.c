
#include <csolve.h>
#include <stdlib.h>

#define INTARR(n) ARRAY VALIDPTR SIZE_GE(4*n)
#define INT2D(x, y) int * INTARR(y) * START INTARR(x)

INT2D(x, y) init_int2d(int REF(V > 0) x, int REF(V > 0) y) OKEXTERN;

int goo(int n, int m) {
  
  int * ARRAY * ARRAY a = init_int2d(n, m);
  
  for (int i = 0; i < n; i++){
    for (int j = 0; j < m; j++){
      a[i][j] = 10;
    }
  }
  
  return 0;

}

int main(){
  
  int n = nondetpos();
  int m = nondetpos();

  //WORKS
  goo(n, m);
  
  //Fails
  int * ARRAY * ARRAY a = init_int2d(n, m);
  for (int i = 0; i < n; i++){
    for (int j = 0; j < m; j++){
      a[i][j] = 10;
    }
  }
  
  return 0;
}
