#include <liquidc.h>

int * LOC(L) x;

void foo (int * LOC(L) y) GLOBAL(L) {
    int *z = nondet () ? x : y;
}

void main () {
    void (*f) (int *);

    f = foo;
    f (0);
}
