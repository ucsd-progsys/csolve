#include <csolve.h>

int x;

void main () {
    x = nondet ();
    if (x > 0) {
        csolve_assert (x > 0);
    }
}
