#include <csolve.h>

void t (int *x) {
    /* csolve_assert (0); */
    *x = 3;
    csolve_assert (0);
}

int main () {
    int x = 0;

    t (&x);

    return 0;
}
