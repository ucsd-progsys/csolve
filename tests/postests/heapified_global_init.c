// Test that a heapified global gets the correct initializer

#include <liquidc.h>

int x = 5;

void main () {
    int *y = &x;

    lcc_assert (*y == 5);
}
