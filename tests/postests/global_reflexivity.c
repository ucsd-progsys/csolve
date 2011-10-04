#include <liquidc.h>

int x;

void main () {
    x = nondet ();
    lcc_assert (x == x);
}
