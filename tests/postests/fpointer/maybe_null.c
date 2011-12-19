#include <csolve.h>

void doNothin () { }

int main () {
    void (*f) ();

    f = nondet () ? 0 : &doNothin;

    if (f != 0)
        f ();

    return 0;
}
