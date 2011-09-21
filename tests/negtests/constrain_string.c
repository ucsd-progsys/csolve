#include <liquidc.h>

void main () {
    char *str = "\0\0";
    lcc_assert (str[0] == 1337);
}
