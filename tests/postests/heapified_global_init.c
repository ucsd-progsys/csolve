// Test that a heapified global gets the correct initializer

#include <csolve.h>

int x = 5;

void main () {
    int *y = &x;

    csolve_assert (*y == 5);
}
