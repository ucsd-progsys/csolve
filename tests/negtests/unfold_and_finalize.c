#include <liquidc.h>

void t (int *x) {
    /* lcc_assert (0); */
    *x = 3;
    lcc_assert (0);
}

int main () {
    int x = 0;

    t (&x);

    return 0;
}
