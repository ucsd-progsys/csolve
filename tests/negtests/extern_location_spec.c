#include <stdlib.h>
#include <liquidc.h>

extern int * START SIZE(4) x CHECK_TYPE OKEXTERN;

void goog () {
    int *y = x;
    if (y != 0) lcc_assert (*y == 0);
}

void foof () {
    int *y = (int *) malloc (sizeof (int));
    *y = 0;
    x = y;

    goog ();
}
