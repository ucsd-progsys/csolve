#include <stdio.h>

/* This is like array1 but we use pointer increment to advance in the array */
#define NR_ITERS    1000
#define ARRAY_SIZE  (1024 * 1024)  // 4 Mb
int array[ARRAY_SIZE];

int main() {
  int acc = 0, iter, i;
  for(iter=0;iter<NR_ITERS;iter++) {
    int * p = array;
    // Just scan the array
    for(i=0;i<ARRAY_SIZE;i++, p++) {
      acc += *p;
    }
  }
  return 0;
}
