//! run with --manual

#include <stdlib.h>

void test (int * ARRAY p) {
    *p = 10;
}

void main () {
    int *q;

    q = (int *)malloc(sizeof(int) * 2);
    q[nondetpos()] = 0;
    test(q);
}
