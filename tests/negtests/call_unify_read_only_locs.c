#include <csolve.h>
#include <stdlib.h>

int read_one_of (int * LOC(P) p, int * LOC(Q) q) {
    return nondet () ? *p : *q;
}

void main () {
    int *a = (int *) malloc (sizeof (int));
    *a = 1;

    int *b = (int *) malloc (sizeof (int));
    *b = 0;

    int *c = nondet () ? a : b;

    csolve_assert (read_one_of (c, c));
}
