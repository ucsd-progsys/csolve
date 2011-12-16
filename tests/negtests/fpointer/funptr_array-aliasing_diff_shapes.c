// run with --manual

#include <csolve.h>
#include <stdlib.h>

void one () { }

void two (int x) { }

typedef void (*voidFunPtr) ();

int main () {
    voidFunPtr *fp = (voidFunPtr *) malloc (sizeof (voidFunPtr) * 2);

    fp[0] = &one;
    fp[1] = &two;

    int i = nondet ();
    if (0 <= i && i < 2) {
        fp[i] ();
    }
}
