#include <csolve.h>
#include <stdlib.h>

void main () {
    int len   = nondetpos ();
    char *str = malloc (len);

    for (int i = 0; i < len - 1; i++) {
        str[i] = 'a';
    }

    for (int j = 0; str[j]; j++)
        ;
}
