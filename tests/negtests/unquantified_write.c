//! run with --manual

#include <stdlib.h>
#include <csolve.h>

int * LOC(L) x;

void foo (int * LOC(L) y) GLOBAL(L) {
    int *z = nondet () ? x : y;
    // pmr: Are globals getting initialized correctly in the scalar stuff?
    /* *z = -1; */
}

void main () {
    x = (int *) malloc (sizeof (int));
    *x = 12;

    int *a = (int *) malloc (sizeof (int));
    *a = -12;

    foo (a);

    csolve_assert (*x > 0);
}
