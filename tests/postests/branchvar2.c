#include <liquidc.h>

void main () {
    int s,t;
    t = nondet();

    switch (t) {
    case 1:
        lcc_assert(t==1);
        s = 0;
        break;
    case 2:
        lcc_assert(t==2);
	s = 1;
        break;
    default:
        s = 2;
    }
}
