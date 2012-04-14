#include <csolve.h>

void foop (int *x, int *c) {
    *x = 0;
    *c = 1;
}

int main (int *p) CHECK_TYPE {
    foop (p, p);

    return 0;
}
