#include <stdlib.h>
#include <liquidc.h>

void main () {
    int  *p;
    int  off;
    char * UNCHECKED c;

    p           = (int *)malloc(4 * 10);
    off         = nondetnn() * 4;
    c           = ((char *)p) + off;
    *((int * UNCHECKED)c) = 0;
}

    
