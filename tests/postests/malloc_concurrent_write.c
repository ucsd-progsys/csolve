#include <cpj.h>
#include <stdlib.h>

void main () {
    int *i = malloc (sizeof (int[2]));

    cobegin
        rtbeg
            i[0] = 1;
        rtend
        rtbeg
            i[1] = 2;
        rtend
    coend
}
