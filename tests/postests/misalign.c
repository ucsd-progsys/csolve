#include <stdlib.h>

struct blerg {
   unsigned short thing_one;
   int thing_two;
};

void foo (struct blerg *y) {
    y->thing_two = 0;
}

void main () {
    struct blerg *y = (struct blerg *) malloc (sizeof (struct blerg));

    foo (y);

    return;
}
