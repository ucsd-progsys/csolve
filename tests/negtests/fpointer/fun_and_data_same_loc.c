#include <csolve.h>
#include <stdlib.h>

void test () {
    return;
}

int main () {
    void (*f) ();
    int *ip = (int *) malloc (sizeof (int));

    *ip = 3;

    if (nondet ()) {
        f = &test;
    } else {
        f = ip;
    }

    return 0;
}
