#include <csolve.h>
#include <cpj.h>
#include <stdlib.h>

void main () {
    int l   = nondetpos ();
    int n   = nondetpos ();
    int len = l + n;
    int *p  = (int *) malloc (len * sizeof(int));

    foreach (i, len - n, l + n)
        p[i] = 0;
        // Sanity check: adding the following makes it unsafe
        /* p[i - 1] = 1; */
    endfor
}
