#include <stdlib.h>

struct pair {
    int x;
    int y;
};

void main() 
{
    struct pair *r = malloc (sizeof (struct pair));

    int zero = 0;
    // If we just make this a constant false branch, CIL optimizes it away
    if (zero == 1)
        r->y = 0;

    return;
}
