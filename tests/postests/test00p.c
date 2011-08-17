#include <stdlib.h>

void main(){
  int *r;
  r = (int*) malloc(sizeof(*r));
  validptr(r);

  *r = 10;
  validptr(r);
  lcc_assert(*r == 10);

  *r = nondet();

  int a = *r;
  int b = *r;
  lcc_assert(a == b);


}
