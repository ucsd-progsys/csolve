#include <liquidc.h>

typedef struct __linkedlist {
    int data;
    struct __linkedlist *next;
} linkedlist;

int x = 5;
int *y;

linkedlist *ll;

void f(linkedlist *lp) {
    lcc_assert(x >= 0);
}

void main () {
    x = 10;
    f(ll);
}
