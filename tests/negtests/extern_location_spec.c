#include <stdlib.h>
#include <csolve.h>

extern int * START SIZE(4) x OKEXTERN;

void goog () {
    int *y = x;
    if (y != 0) csolve_assert (*y == 0);
}

void foof () {
    int *y = (int *) malloc (sizeof (int));
    *y = 0;
    x = y;

    goog ();
}
