#include <liquidc.h>

void main () {
    int x = nondet();
    assert (x & 1);
}
