#include <stdlib.h>
#include <csolve.h>

struct foo {
  int data;
  struct foo * ARRAY next;
};

struct foo * ARRAY main(){
  struct foo *a;
  struct foo *vt;
  struct foo *b;

  int i;
  int n;

  n  = nondetpos();
  a  = (struct foo *) malloc(n * sizeof(struct foo));
  vt = (struct foo *) 0;
  
  i = n-1;
  while (i >= 0){
    b 	    = a + i;
    b->data = 99999;
    b->next = vt;
    vt = b;
    i--;
  }

  for (i=0; i < n; i++){
    b = a[i].next;
    if (b != 0){
      validptr(b);
    }
  }

  return a;
}
