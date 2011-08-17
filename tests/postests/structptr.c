#include <stdlib.h>

typedef struct __str__ {
    int x;
    int y;
} str;

void main () {
    str *s;
    s = nondet() ? (str *)malloc(sizeof(str)) : (str *)0;
    if (s == (str *)0) { DIVERGE: goto DIVERGE; }
    s->x = 0;
    validptr(&(s->y));
}
