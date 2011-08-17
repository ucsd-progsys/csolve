#include <liquidc.h>

int foo () {
    return 0;
}

int bar () {
    return 1;
}

int main () {
    int (*f)();

    f = nondet () ? &foo : &bar;
    int z = f ();
    assert (z >= 0);

    return 0;
}
