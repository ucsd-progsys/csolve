#include <csolve.h>

int p[] = { 0, 1, 2 };
int *a;

void f() {
    p[0] = 1000;
}

int main () {
  int x;
    
  x = *p;
  
  f();
  
  csolve_assert (x <= 1000);
}
