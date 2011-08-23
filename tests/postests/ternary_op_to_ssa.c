#include <liquidc.h>

void main () {
    // If we declare i and then set it later, either in the if or with the
    // ternary op, then everything works ok.
    /* int i; */

    /* if (nondet ()) { */
    /*     i = 0; */
    /* } else { */
    /*     i = 0; */
    /* } */

    int i = nondet () ? 0 : 0;

    lcc_assert (i == 0);
}
