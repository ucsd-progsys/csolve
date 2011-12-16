#include <csolve.h>

int (*f) ();

int one () {
    return 0;
}

int main () {
    f = &one;
    csolve_assert (f () > 0);

    return 0;
}
