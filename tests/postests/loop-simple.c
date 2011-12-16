#include <csolve.h>

int main(){
  int i, n; 
  n = nondet();
  for (i=0; i < n; i++){
    nondet();
  }
  csolve_assert (i >= 0);
  return 0;
}
