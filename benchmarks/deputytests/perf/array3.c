#include <stdio.h>
#include "../nodeputy.h"

int localfunc(int * BND(p, end) p, int * SNT end) {
  int acc = 0;
  if (!p) return 0;
  while (p < end) {
    acc += *p;
    p++;
  }
  return acc;
}

#define ARRAY_SIZE 16384
int main() {
  int i;
  int array[ARRAY_SIZE];
  for(i=0;i<100000;i++) {
    localfunc(array, array+ARRAY_SIZE);
  }
  return 0;
}
