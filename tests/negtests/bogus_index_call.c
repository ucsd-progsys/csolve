#include <stdlib.h>
#include <csolve.h>

extern void solveHaltingProblem (int REF(V >= 0) tm) OKEXTERN;

int main () {
    solveHaltingProblem (nondet ());

    return 0;
}
