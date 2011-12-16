#include <csolve.h>

void test () {
    return;
}

void test2 () {
    return;
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
