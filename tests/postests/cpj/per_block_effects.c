#include <cpj.h>

void write (int * ARRAY p) {
    int i = nondet ();
    CSOLVE_ASSUME (0 <= i);
    CSOLVE_ASSUME (i <= 1);

    cobegin
        rtbeg
            p[i] = 10;
        rtend
        rtbeg
            p[1 - i] = 20;
        rtend
    coend
}

void main (int * ARRAY ROOM_FOR(int[2]) VALIDPTR START p) CHECK_TYPE {
    write (p);
}
