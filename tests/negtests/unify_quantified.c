#include <csolve.h>

void foo (int * LOC(L) x, int * LOC(K) y) CHECK_TYPE {
    int *p = nondet () ? x : y;
    *p = 0;
}
