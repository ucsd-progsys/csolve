#include <csolve.h>
#include <stdlib.h>

// Want this to work whether the alloc happens from a function or not --- what if
// we allocate locally? Do we get the false'd location inside the
// function?
int *dont_alloc () {
    return NULL;
}

int *alloc () {
    int *ret = (int *) malloc (sizeof (int));

    *ret = 3;
    
    return ret;
}

void main () {
    int *p = nondet () ? alloc () : dont_alloc ();

    if (p) {
        csolve_assert (*p == 3);
    }
}
