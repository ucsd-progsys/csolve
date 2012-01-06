#include <stdlib.h>
#include <csolve.h>

int *make () {
  int *p = malloc (sizeof (int));
  // Breaking control flow between malloc and assignment used to result
  // in a spurious generalization. (One could also break with an "if" here.)
  { __blockattribute__ ((break_block)) }
  *p = 1;
  return p;
}

void main () {
  int *p = make ();
  csolve_assert (*p);
}
