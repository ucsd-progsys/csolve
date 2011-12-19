#include <csolve.h>

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
    csolve_assert (t.c == 1);

    return;
}
