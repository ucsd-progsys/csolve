#include <stdlib.h>
#include <liquidc.h>

int a[] = { 0, 1, 2 };

void incGlobal () {
    a[0]++;
}

int main () {
    void (*f) ();
    int *x;

    f = &incGlobal;
    f ();
    lcc_assert (*a >= 0);

    return 0;
}
