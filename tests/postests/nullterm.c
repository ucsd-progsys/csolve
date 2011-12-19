#include <csolve.h>
#include <stdlib.h>

void main () {
    int len   = nondetpos ();
    char *str = malloc (len);

    for (int i = 0; i < len - 1; i++) {
        str[i] = 'a';
    }

    while (1) {
        char c = *str;
        if (!c) break;

        *str = *str + 1;
        str++;
    }
}
