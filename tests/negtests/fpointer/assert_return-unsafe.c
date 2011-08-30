#include <liquidc.h>

int two () {
    return 0;
}

int main () {
    int (*f) () = &two;
    int x = f ();
    lcc_assert (x > 0);

    return 0;
}
