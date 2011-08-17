#include <liquidc.h>

void main(){
  int i, p, q; 
  int n;

  n = nondet();
  q = nondet();

  p = q;
  i = 0;
  
  for(; i < n; i++){
    p++;
  }
 
  lcc_assert(p == q+i);

  return;
}
