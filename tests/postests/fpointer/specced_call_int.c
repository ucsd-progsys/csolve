#include <liquidc.h>

typedef int REF(V > 0) (* VALIDPTR fptr) ();

void call_it (fptr f) CHECK_TYPE {
    int z = f();
    lcc_assert (z > 0);
}
