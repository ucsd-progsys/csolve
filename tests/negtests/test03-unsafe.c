#include <liquidc.h>

int abs(int x){
  if (x > 0){		
    return x;
  } else {
    return (0-x);
  }
}

void main(){
  int x;
  int y;
  x = nondet(); 
  y = abs(x);

  lcc_assert(y >= 10);
  return;
}
