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
    assert (t.c == 1);

    return;
}
