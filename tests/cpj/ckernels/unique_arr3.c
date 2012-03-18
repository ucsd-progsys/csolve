#include <csolve.h>
#include <stdlib.h>

#define NINTS 3

int ** ARRAY create () {
  int **arr = malloc(NINTS * sizeof (int*));
  int i     = nondetnn ();
  CSOLVE_ASSUME (i < NINTS);
  int *x    = malloc (sizeof (int));
  *x        = i;
  arr[i]    = x;

  return arr;
}

void main () {
  int **arr = create ();

  int *x = arr[1];

  if (x != NULL) {
    csolve_assert (*x == 1);
  }
}
