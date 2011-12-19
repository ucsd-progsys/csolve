#include <csolve.h>
#include <stdlib.h>

int * LOC(L) x;

void foo (int * LOC(L) *y) GLOBAL(L) {
    *y = x;
}

void main () {
    int **z = malloc (sizeof (int *));

    foo (z);
}
