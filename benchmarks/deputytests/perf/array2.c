#include <stdio.h>

#ifndef DEPUTY
#define DEPUTY_NUM_CHECKS_ADDED 0
#endif

#define SIZE 16384
int localfunc() {
  // A local array of integers
  int array[SIZE];
  int acc = 0;
  int i;
  
  for(i=0;i<SIZE;i++) {
    acc += array[i];
  }
  return acc;
}

int main() {
  int i;
  for(i=0;i<100000;i++) {
    localfunc();
  }
  //All checks in this program should have been optimized away.
  if (DEPUTY_NUM_CHECKS_ADDED != 0) {
    return 3;
  }
  return 0;
}
