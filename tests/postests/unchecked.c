#include <stdlib.h>
#include <csolve.h>

typedef struct __stuff__ {
    int x;
} stuff;

typedef struct __bigstuff__ {
    int a;
    int b;
} bigstuff;

void main () {
    int      * UNCHECKED x;
    stuff                                * y;
    bigstuff * UNCHECKED z;

    x     = (int * UNCHECKED)malloc(10);
    x[10] = 0;

    y    = (stuff *)malloc(sizeof(stuff));
    z    = (bigstuff * UNCHECKED)y;
    z->b = 0;
}
