#include <csolve.h>

int * LOC(L) x;

void foo (int * LOC(L) y) GLOBAL(L) {
    int *z = nondet () ? x : y;
}

void main () {
    int *w = 0;

    foo (w);
}
