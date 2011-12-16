#include <stdlib.h>

int main () {
    int *x = (int *)malloc (sizeof (int));
    *x = nondet ();

    int *y = (int *)malloc (sizeof (int));
    *y = nondet ();

    if (nondet ()) {
        x = y;
    }

    if (x == y) {
        csolve_assert (*x == *y);
    }
}
