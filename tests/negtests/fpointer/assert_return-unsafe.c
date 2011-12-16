#include <csolve.h>

int two () {
    return 0;
}

int main () {
    int (*f) () = &two;
    int x = f ();
    csolve_assert (x > 0);

    return 0;
}
