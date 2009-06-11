#include <stdio.h>
#include <stdlib.h>

// Matrix multiplication where a matrix is an array of pointers
#ifndef DEPUTY
   #define COUNT(x)
   #define NT
   #define NTS
   #define NONNULL
#endif
#define MATXSIZE 500
typedef double * COUNT(MATXSIZE) * matx_t;

// a = mn
void matxmult(int sz, matx_t COUNT(sz) a, matx_t COUNT(sz) m, matx_t COUNT(sz) n) {
  int i,j,k;
  for(i = 0; i < sz; i++) {
    for(j = 0; j < sz; j++) {
      for(k = 0; k < sz; k++) {
	a[i][j] += m[i][k] * n[k][j];
      }
    }
  }
}

void mkmatx(int sz, matx_t COUNT(sz) x) {
  int i,j;
  for(i = 0; i < sz; i++) {
    for(j = 0; j < sz; j++) {
      x[i][j] = 1.0;
    }
  }
}

matx_t COUNT(MATXSIZE) initmatx(int sz) {
  int i,j;
  matx_t ptr = (matx_t)malloc(sizeof(*ptr)*sz);
  for(i = 0; i < sz; i++) {
    ptr[i] = (double*)malloc(sizeof(double)*sz);
    for(j = 0; j < sz; j++) {
      ptr[i][j] = (double)0.0;
    }
  }
  return ptr;
}

int main(int argc, char* NTS NONNULL * COUNT(argc) NONNULL argv) {
  int i;
  int sz = MATXSIZE;
  matx_t x, y, a;
  x = initmatx(sz);
  y = initmatx(sz);
  a = initmatx(sz);

  mkmatx(sz,x);
  mkmatx(sz,y);
  matxmult(sz,a,x,y);
  return 0;
}
