#include <csolve.h>

int x;

void main () {
    x = nondet ();
    csolve_assert (x == x);
}
