#include <liquidc.h>

typedef struct {
    int a;
} first;

typedef struct {
    first f;
    char b;
} second;

typedef struct {
    second s;
    short c;
} third;

third t = { { { 0 }, 1 }, 2 };

void main () {
    lcc_assert (t.s.f.a == 0);
    lcc_assert (t.s.b   == 1);
    lcc_assert (t.c     == 2);

    return;
}
