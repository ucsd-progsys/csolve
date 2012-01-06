#include <csolve.h>
#include <stdio.h>

void foo (const char * f) {
}

void main () {
  void (*fp) (const char *) = foo;

  fprintf (stdout, "a");
  fp ("b");
}
