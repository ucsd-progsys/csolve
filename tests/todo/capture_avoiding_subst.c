#include <csolve.h>

typedef void fp (char * LOC(L) nptr);
extern fp *foo OKEXTERN;
extern void bar (char * LOC(L), fp *) OKEXTERN; 

void main (char *argv) {
  bar (argv, foo);
}
