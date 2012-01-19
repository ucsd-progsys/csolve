#include <csolve.h>

int * LOC(L) x;
char * LOC(K) c;

void one (int * LOC(L) a, char * LOC(K) b) GLOBAL(L) GLOBAL(K) {
    int *aa  = nondet () ? a : x;
    char *cc = nondet () ? c : b;
}

void two (int * LOC(L) t, char * LOC(K) u) GLOBAL(L) GLOBAL(K) {
    int *tt  = nondet () ? t : x;
    char *cc = nondet () ? c : u;
}

void main () {
  void (GLOBAL(L) GLOBAL(K)*f) (int * LOC(L), char * LOC(K));

    f = nondet () ? one : two;
    f (0, 0);
}
