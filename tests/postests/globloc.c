#include <liquidc.h>

int p[] = { 0, 1, 2 };
int *a;

void f() {
    p[0] = 1000;
}

int main () {
  int x;
    
  x = *p;
  
  f();
  
  assert (x <= 1000);
}
