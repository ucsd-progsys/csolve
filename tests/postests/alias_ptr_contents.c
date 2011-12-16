#include <stdlib.h>

int main () {
    int *x = (int *) malloc (sizeof (int));
    *x = nondet ();

    int *y = (int *) malloc (sizeof (int));
    *y = nondet ();

    int **p = (int **) malloc (sizeof (int *));
    *p = x;

    int **q = (int **) malloc (sizeof (int *));
    *q = y;

    if (nondet ()) {
        p = q;
    }

    if (p == q) {
        csolve_assert (*p == *q);
    }
    
    return 0;
}
