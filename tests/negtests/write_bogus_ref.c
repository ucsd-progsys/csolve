#include <stdlib.h>

void test (int **p) {
    int *q = (int *) malloc (sizeof (int));

    while (1) {
        *p = q;
        q++;
    }
}
