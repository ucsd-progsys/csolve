#include <cpj.h>

void main (int * ARRAY ROOM_FOR(int[2]) VALIDPTR START p) CHECK_TYPE {
    foreach (i, 0, 2)
        p[i] = 0;
        // Sanity check: adding the following makes it unsafe
        /* p[i - 1] = 1; */
    endfor
}
