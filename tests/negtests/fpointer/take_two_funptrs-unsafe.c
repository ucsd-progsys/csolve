#include <csolve.h>

void test () {
    return;
}

int test2 () {
    return 0;
}

int main () {
    void (*f)();

    if (nondet ()) {
        f = &test;
    } else {
        f = &test2;
    }

    return 0;
}
