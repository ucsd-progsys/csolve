#include <csolve.h>
#include <stdlib.h>

void foo (int y, int *q) {
    lcc_assert (*q == y);
}

void main(void) {
  int x  = nondetpos ();
  int *p = malloc (sizeof (int));
  *p = x;

  foo (x, p);
}
