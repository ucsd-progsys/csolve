#include <liquidc.h>

void main () {
    int x = nondet();
    lcc_assert (x & 1);
}
