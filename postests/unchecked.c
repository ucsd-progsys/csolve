extern void *malloc(int);

typedef struct __stuff__ {
    int x;
} stuff;

typedef struct __bigstuff__ {
    int a;
    int b;
} bigstuff;

void main () {
    int      * __attribute__ ((unchecked)) x;
    stuff                                * y;
    bigstuff * __attribute__ ((unchecked)) z;

    x     = (int * __attribute__ ((unchecked)))malloc(10);
    x[10] = 0;

    y    = (stuff *)malloc(sizeof(stuff));
    z    = (bigstuff * __attribute__ ((unchecked)))y;
    z->b = 0;
}
