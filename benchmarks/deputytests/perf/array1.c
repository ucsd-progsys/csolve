#include <stdio.h>

#ifndef DEPUTY
#define DEPUTY_NUM_CHECKS_ADDED 0
#endif

// Some performance tests
// Reading from a SEQ pointer

#define NR_ITERS    1000
#define ARRAY_SIZE  (1024 * 1024)  // 4 Mb
int array[ARRAY_SIZE];

int main() {
  int acc = 0, iter, i;
  int * p = array;
  for(iter=0;iter<NR_ITERS;iter++) {
    // Just scan the array
    for(i=0;i<ARRAY_SIZE;i++) {
      acc += p[i];
    }
  }
  //All checks in this program should have been optimized away.
  if (DEPUTY_NUM_CHECKS_ADDED != 0) {
    return 3;
  }
  return 0;
}


