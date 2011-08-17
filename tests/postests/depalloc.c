#include <stdlib.h>

char *allocstr(int n) {
    char *s;

    s  = (char *)malloc(n * sizeof(char));
    *s = 0; // SINFER ISSUE

    return s;
}

void main () {
    int n = nondetpos ();
    char *s;

    s = allocstr(n);
    validptr(s);
}
