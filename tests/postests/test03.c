#include <csolve.h>

int max (int x, int y){
  if (x > y) {
    return x;
  } else {
    return y;
  }
}

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
  int z;

  x = nondet(); 
  y = abs(x);
  z = max(x, 0);

  csolve_assert(y >= 0);
  csolve_assert(z >= 0);
  
  return;
}
