#include <csolve.h>
#include <stdlib.h>

#define NINTS 3

int ** ARRAY create () {
  int **arr = malloc(NINTS * sizeof (int*));

  for (int i = 0; i < NINTS; i++) {
    int *x    = malloc (sizeof (int));
    *x        = i;
    arr[i]    = x;
  }

  return arr;
}

void main () {
  int **arr = create ();

  for (int i = 0; i < NINTS; i++) {
    int *x = arr[i];
    if (x != NULL) {
      csolve_assert (*x == i);
    }
  }
}
