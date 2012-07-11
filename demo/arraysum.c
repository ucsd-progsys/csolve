// This program demonstrates two different methods for computing
// the sum of an array in place in parallel.
//
// CSolve proves the program behaves deterministically when
// executed in parallel.

#include <cpj.h>
#include <stdlib.h>

#define THREADS 8

void sumBlock (char * ARRAY a, int i, int len) {
  if (len <= 1) return;

  int hl = len / 2;

  cobegin
    rtn (sumBlock (a, i, hl))
    rtn (sumBlock (a, i + hl, len - hl))
  coend

  a[i] += a[i + hl];
}

int sum1 (char * ARRAY START NONNULL SIZE(len) a, int REF(V > 0) len)
  CHECK_TYPE
{
  sumBlock (a, 0, len);

  return a[0];
}

void sumStride (char * ARRAY a, int stride) {
  foreach (i, 0, THREADS)
    for (int j = i; j < stride; j += THREADS) {
        a[j] = a[j + stride];
    }
  endfor
}

int sum2 (char * ARRAY START NONNULL SIZE(len * 4) a, int REF(V > 0) len)
  CHECK_TYPE
{
  for (int stride = len / 2; stride > 0; stride /= 2)
    sumStride (a, stride);

  return a[0];
}
