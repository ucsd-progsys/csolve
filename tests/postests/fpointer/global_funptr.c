#include <liquidc.h>

int (*f) ();

int one () {
    return 1;
}

int main () {
    f = &one;
    int (*g)() = f;

    if (g != 0)
        lcc_assert (g () > 0);

    return 0;
}
