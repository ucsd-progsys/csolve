#include <stdlib.h>

typedef int * ARRAY LOC(L) int_array;

int sum(int_array INST(L, L) buf, int_array INST(L, L) end){
  int sum = 0;
  while (buf <= end){
    validptr(buf);
    sum += *buf;
    buf++;
  }
  return sum;
}

int main(){
  int *x;
  int *z;
  int n,i,j,k;
  int res;
  int tmp;
  int foo;

  n = nondetpos();
  z = (int *) malloc(n * sizeof(int));
  
  x = z;
  int i = 0;
  
  for (; i < n; i++){  
	foo = i;
    validptr(x);
    tmp = nondet();
    *x  = tmp;
    x++;
  }

  int j = nondetpos();
  int k = nondetpos();
  if (j < k && k < n){
    validptr(z+j);
    validptr(z+k);
    res = sum(z+j, z+k);
  }

  return 0;
}
