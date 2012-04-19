#include <csolve.h>

int *p;

void main (int *q) {
    int *r = nondet () ? p : q;
}
