#include <csolve.h>

typedef int REF(V > 0) (* VALIDPTR fptr) ();

void call_it (fptr f) CHECK_TYPE {
    int z = f();
    csolve_assert (z > 0);
}
