#include <stdlib.h>

void validptr (void *p) {
    return;
}

int nondet () {
    return 0;
}

int nondetnn () {
    return 0;
}

int nondetpos () {
    return 1;
}

int assert (int p) {
    if (!p) {
        exit (0);
    }

    return 0;
}

int assume (int p) {
    while (!p) {
        ;
    }

    return 0;
}

int bor (int a, int b) {
    return a | b;
}

int band (int a, int b) {
    return a & b;
}
