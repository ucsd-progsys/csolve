#include <csolve.h>
#include <stdlib.h>

void main () {
    int *p = (int *) malloc (sizeof (int) * 100);
    int i  = nondet ();
    if (i < 100) {
        p[i + 3] = 0;
    }
}
