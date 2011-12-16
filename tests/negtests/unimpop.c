#include <liquidc.h>

void main () {
    int x = nondet();
    csolve_assert (x & 1);
}
