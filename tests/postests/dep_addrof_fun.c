

#include <stdlib.h>

extern void foo(int x, int REF(PNN(V > x)) *y) OKEXTERN;

void helper(int i){
  int z = i + 1;
  foo(i, &z);
} 

void main(){
  for(int i = 0; i < 100; i++)
    helper(i); 
}



