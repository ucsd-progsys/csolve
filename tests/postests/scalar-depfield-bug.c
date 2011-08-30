#include <stdlib.h>

typedef struct {
    int x;
} str;

void test(str *s) {
    int x;
    s = s; // Remove this and the problem goes away
    x = s->x;
}

void main() {
    str *s = (str *) malloc (sizeof (str));
    test (s);
}
