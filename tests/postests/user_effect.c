#include <stdlib.h>
#include <cpj.h>

CSOLVE_EFFECT(ELaunchMissiles)
CSOLVE_EFFECTS_COMMUTE(ELaunchMissiles, ELaunchMissiles)

extern void harmless (int * LOC(L) x)
    EFFECT (L, && [V = x; ELaunchMissiles = 1; EWRITE != 1; EREAD != 1])
    OKEXTERN;

void main () {
    int *x = (int *) malloc (sizeof (int));

    cobegin
        rtn(harmless (x))
        rtn(harmless (x))
    coend
}
