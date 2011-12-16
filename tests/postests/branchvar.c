#include <csolve.h>

void main () {
    int s;
    switch (nondet()) {
    case 1:
        s = 0;
        break;
    case 2:
        s = 1;
        break;
    default:
        s = 2;
    }
}
