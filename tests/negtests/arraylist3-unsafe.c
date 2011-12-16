//This is actually safe but requires "range-predicates" like reasoning
//beyond our current system

#include <stdlib.h>
#include <csolve.h>

struct foo {
  int data;
};

struct foo *__attribute__((array)) main(){
  struct foo *a;
  struct foo *b;

  int i,j;
  int n;

  n  = nondetpos();
  a  = (struct foo *) malloc(100 * sizeof(struct foo));
 
  i = n-1;
  while (i >= 0){
    b 	    = a + i;
    b->data = 999;
    i--;
  }

  for (j=0; j < n; j++){
    b       = a + j;
    csolve_assert(b->data == 999);
  }

  return a;
}
