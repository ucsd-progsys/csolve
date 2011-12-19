#include <stdlib.h>
#include <csolve.h>

void main () {
    int  *p;
    int  off;
    char * UNCHECKED c;

    p           = (int *)malloc(4 * 10);
    off         = nondetnn() * 4;
    c           = ((char *)p) + off;
    *((int * UNCHECKED)c) = 0;
}

    
