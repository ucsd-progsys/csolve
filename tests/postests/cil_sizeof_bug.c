// Tickles a bug (?) in CIL's simplemem: memory dereferences in sizeof
// expressions are expanded into a series of memory accesses; K&R says
// the expression is not to be evaluated.

#include <stdlib.h>

struct hash {
   int * ARRAY array ;
};

typedef struct hash *Hash;

void main () {
    Hash retval;
  
    retval        = (struct hash *) malloc (sizeof (Hash));
    retval->array = (int *) malloc (sizeof (*retval->array));
}
