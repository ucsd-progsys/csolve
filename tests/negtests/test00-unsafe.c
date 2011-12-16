#include <csolve.h>

void main(){
  int i;

  i = 0;
  while (nondet()){
    i++;
  }

  csolve_assert (i < 0);
}
