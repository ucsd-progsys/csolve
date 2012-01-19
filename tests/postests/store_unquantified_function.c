#include <csolve.h>

int * LOC(L) x;

void foo (int * LOC(L) y) GLOBAL(L) {
    int *z = nondet () ? x : y;
}

void main () {
  void (GLOBAL(L) *f) (int * LOC(L));

    f = foo;
    f (0);
}
