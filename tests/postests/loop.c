#include <csolve.h>

int main(int REF(V > 10) n) CHECK_TYPE {
  int i, z; 
  //n = nondet();
  csolve_assert(10 < n);

  z = 0;
  for (i=0; i < n; i++){
    z += i;
    z += i;
    z += i;
    z += i;
  }
  csolve_assert (z >= 0);
  return 0;
}
