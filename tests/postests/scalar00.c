#include <liquidc.h>

int bar(int y){
  return 0;
}

int * LOC(L) foo(int * LOC(L) x, int *__attribute__((array)) y){
  return x;
}

void main(){
  int x;
  
  int z = 100;

/* 
  x = nondet();

  if (x < 0) {
    x = -x;
  }

  lcc_assert(x >= 0);
*/
  return; 
}
