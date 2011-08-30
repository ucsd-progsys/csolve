#include <stdlib.h>

void main () {
    int *p = (int *) malloc (sizeof (int));
    int   i;

    for (i = 10; i < 100; i++)
        p[i] = 0;

    char *jp = (char *)p;
    jp[42] = 0;
}
