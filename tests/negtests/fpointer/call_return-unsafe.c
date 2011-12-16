#include <csolve.h>

int one () {
    return 1;
}

int two () {
    return 0;
}

void invoke (int (*f) ()) {
    int z = f ();
    csolve_assert (z > 0);
}

int main () {
    int (*f) ();

    f = nondet () ? &one : &two;
    invoke (f);

    return 0;
}
    
