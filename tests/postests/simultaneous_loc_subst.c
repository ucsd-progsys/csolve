#include <csolve.h>

struct foo {
    int x;
    int y;
};

void set (int * LOC(L) a, struct foo * LOC(J) b) {
    b->y = *a;
}

// The call creates the *simultaneous* substitution [L -> J, J -> K].
// If this substitution is treated instead as the first part composed
// onto the second, an error occurs.
void test (int * LOC(J) a, struct foo * LOC(K) b) CHECK_TYPE {
    set (a, b);
}
