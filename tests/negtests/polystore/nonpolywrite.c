//! run with --notruekvars
#include <csolve.h>
#include <stdlib.h>

void foo(void **p)
{
  int *x = malloc(sizeof(*x));
  *x = 3;
  *p = x;
}
