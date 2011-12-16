#include <csolve.h>

int maim () {
    int a = nondetpos ();
    int b = nondetpos ();

    while (a > 0 && b > 0) {
        a--; b--;
    }

    if (a > 0) {
        csolve_assert (b <= 0);
    }

    return 0;
}
