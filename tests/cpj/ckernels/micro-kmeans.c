#include <liquidc.h>

extern int REF(V = z) * NONNULL START ROOM_FOR(int) mallocAndSet (int z) OKEXTERN;

void foo (int y, int *q) {
    lcc_assert (*q == y);
}

void main(void) {
  int x  = nondetpos ();
  int *p = mallocAndSet (x);

  foo (x, p);
}
