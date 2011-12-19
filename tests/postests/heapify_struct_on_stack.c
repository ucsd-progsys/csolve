#include <stdlib.h>
#include <csolve.h>

struct tokenbuffer
{
  char * buffer;
};

typedef struct tokenbuffer token_buffer;

void main () {
    token_buffer tb;

    tb.buffer = NULL;
    csolve_assert (tb.buffer == NULL);
}
