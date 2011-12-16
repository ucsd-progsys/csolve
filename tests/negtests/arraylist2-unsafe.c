#include <stdlib.h>
#include <csolve.h>

struct foo {
  int data;
  struct foo *__attribute__((array)) next;
};

struct foo *__attribute__((array)) main(){
  struct foo *a;
  struct foo *vt;
  struct foo *b;

  int i;
  int n;

  n  = nondetpos();
  a  = (struct foo *) malloc(100 * sizeof(struct foo));
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
    csolve_assert (a[i].data == 10);
    b = a[i].next;
    if (b != 0){
      validptr(b);
    }
  }

  return a;
}
