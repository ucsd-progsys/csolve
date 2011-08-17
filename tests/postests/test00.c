#include <stdlib.h>

int bar(int y){
  return 0;
}

int * LOC(L) foo(int * LOC(L) x, int *__attribute__((array)) y){
  return x;
}

void main(){
  int x;
  int a;
  int b;

  x = nondet();


  if (x < 0) {
    x = -x;
  }

  a = (x >= 0);
  
  a = 999;

  lcc_assert(x >= 0);

  lcc_assert(x >= 0);

  return; 
}
