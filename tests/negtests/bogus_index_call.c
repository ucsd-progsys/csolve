#include <stdlib.h>
#include <liquidc.h>

extern void solveHaltingProblem (int REF(V >= 0) tm) OKEXTERN;

int main () {
    solveHaltingProblem (nondet ());

    return 0;
}
