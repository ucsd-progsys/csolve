#include <stdlib.h>

void main(){
  int *r;
  r = (int*) malloc(sizeof(*r));
  validptr(r);

  *r = 10;
  validptr(r);
  csolve_assert(*r == 10);

  *r = nondet();

  int a = *r;
  int b = *r;
  csolve_assert(a == b);


}
