#include <liquidc.h>
#include <stdlib.h>

int read_one_of (int * LOC(P) p, int * LOC(Q) q) {
    return nondet () ? *p : *q;
}

void main () {
    int *a = (int *) malloc (sizeof (int));
    *a = 1;

    lcc_assert (read_one_of (a, a));
}
