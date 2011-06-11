#include <stdio.h>
#include <stdlib.h>

int fib (void *p, int i) {
  int (*foo)(void *, int) = (int (*)(void *, int)) p;
  if (i == 0) return 1;
  if (i == 1) return 1;
  return foo(foo, i-1) + foo(foo, i-2);
}


int fibWrapper (int (*fp)(void *, int), int i) {
  return fib(fp, i);
}

int main (int argc, char *argv[]) {
  int x;

  if (argc > 1) { 
    x = atoi(argv[1]);
  } else {
    x = 10;
  }
  printf ("fib of %d is %d\n", x, fibWrapper(&fib, x));
  return 0;
}
