#include <csolve.h>
#include <stdlib.h>

typedef void (*setter) (int *);

void one (int *x) {
    *x = 1;
}

void two (int *y) {
    *y = 2;
}

void invoke (setter f) {
    int *z = (int *) malloc (sizeof (int));

    f (z);
    csolve_assert (*z >= 0);
}

void main () {
    setter f;

    f = nondet () ? &one : &two;
    invoke (f);

    return;
}
