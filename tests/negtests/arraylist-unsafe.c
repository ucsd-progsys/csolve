#include <stdlib.h>
#include <csolve.h>

struct foo {
  int data;
//  struct foo *next;
};

struct foo *__attribute__((array)) main(){
  struct foo *a;
  struct foo *b;

  int i;
  int n;

  n  = nondetpos();
  a  = (struct foo *) malloc(n * sizeof(struct foo));
 
  i = n-1;
  while (i >= 0){
    b 	    = a + i;
    b->data = 99999;
    i--;
  }

  for (i=0; i < n; i++){
    b = a + i;
    csolve_assert(b->data == 0);
  }

  return a;
}
