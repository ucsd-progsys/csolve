#include <csolve.h>
#include <stdlib.h>

#define NINTS 3

void test (int ** ARRAY arr) {
  int *x = arr[1];

  if (x != NULL) {
    csolve_assert (*x == 1);
  }
}

void main () {
  int **arr = malloc (NINTS * sizeof (*arr));
  int *x    = NULL;
 
  for (int i = 0; i < NINTS; i++) {
    x      = malloc (sizeof (*x));
    // Where the hell is the constraint for this write??
    *x     = i;
    arr[i] = x;
  }

  test (arr);
}
