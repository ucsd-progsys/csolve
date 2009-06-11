#include <stdio.h>
#include <stdlib.h>

// Matrix multiplications with square matrices
//
#ifndef DEPUTY
  #define COUNT(n)
  #define NONNULL
#endif

#define SIZE 100
typedef int matx[SIZE][SIZE];

int mult(matx COUNT(SIZE) a,
         matx COUNT(SIZE) b,
         matx COUNT(SIZE) c) {
  int i, j, k;
  for(i=0;i<SIZE;i++) {
    for(j=0;j<SIZE;j++) {
      int acc = 0;
      for(k=0;k<SIZE;k++) {
        c[i][j] += a[i][k] * b[k][j];
      }
    }
  }
}

int main() {
  int i;
  matx * a = (matx *)malloc(sizeof(matx));
  matx * b = (matx *)malloc(sizeof(matx));
  matx * c = (matx *)malloc(sizeof(matx));
  
  for(i=0;i<300;i++) {
    mult(*a, *b, *c);
  }
}
