#include <liquidc.h>

void main(){
  int i, z, n; 
  n = nondet();
  z = 0;
  for (i=0; i < n; i++){
    z -= i;
  }
  lcc_assert (z >= 0);
}
