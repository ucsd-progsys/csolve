#include <liquidc.h>

int x;

void main () {
    x = nondet ();
    if (x > 0) {
        lcc_assert (x > 0);
    }
}
