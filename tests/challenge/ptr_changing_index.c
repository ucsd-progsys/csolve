// Taken from coreutils/src/tsort.c

#include <stdlib.h>

struct successor {
  struct successor *next;
};

// Swapping count and top makes this have a compatible layout with
// successor
struct item {
  int count;
  struct successor *top;
};

void main () {
    struct item *k = malloc (sizeof (struct item));
    k->count = 0;
    struct successor **p = &k->top; // Offset 4 from start
    
    while (1) {
        p = &(*p)->next;            // Offset 0 from start
    }
}
