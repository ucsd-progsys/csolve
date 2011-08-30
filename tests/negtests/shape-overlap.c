#include <stdlib.h>

void main () {
    char *p = (char *) malloc (100);
    int   i;

    for (i = 10; i < 100; i++)
        p[i] = 0;

    int *jp = (int *)p;
    jp[2] = 0;
}
