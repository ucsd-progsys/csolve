#include <liquidc.h>

void main(){
  int i;

  i = 0;
  while (nondet()){
    i++;
  }

  lcc_assert (i < 0);
}
