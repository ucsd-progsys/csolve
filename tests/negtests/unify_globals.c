#include <csolve.h>

int * LOC(L) p;
int * LOC(K) q;

void main () {
    int *k = nondet () ? p : q;
}
