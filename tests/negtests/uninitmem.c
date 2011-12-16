#include <stdlib.h>
#include <liquidc.h>

void f (int *y) {
    int x = *y;

    csolve_assert(0);
}

void main () {
    int *z;

    z = (int *)malloc(sizeof(int));
    f(z);
}
