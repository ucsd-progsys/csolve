#include <stdlib.h>
#include <csolve.h>

int a[] = { 0, 1, 2 };

void decGlobal () {
    a[0]--;
}

int main () {
    void (*f) ();
    int *x;

    f = &decGlobal;
    f ();
    csolve_assert (*a >= 0);

    return 0;
}
