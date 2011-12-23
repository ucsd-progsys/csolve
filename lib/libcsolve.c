#include <stdlib.h>

void csolve_fold_all () {
}

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

int nondetrange (int l, int u) {
    return l;
}

int csolve_assert (int p) {
    if (!p) {
        exit (1);
    }

    return 0;
}

int csolve_assume (int p) {
    while (!p) {
        ;
    }

    return 0;
}

void csolve_block_begin (void *p) {
    csolve_assert (0);
}

void csolve_block_end (void *p) {
    csolve_assert (0);
}

int csolve_mod (int a, int m) {
    return a % m;
}

int bor (int a, int b) {
    return a | b;
}

int band (int a, int b) {
    return a & b;
}
