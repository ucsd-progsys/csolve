#include <cpj.h>
#include <stdlib.h>

CSOLVE_EFFECT(EAccumulate)
CSOLVE_EFFECTS_COMMUTE(EAccumulate, EAccumulate)

extern void accumulate (char * LOC(L) ARRAY l, int i)
  EFFECT(L, && [V = l + i; EAccumulate = 1; ~(EWRITE = 1); ~(EREAD = 1)])
  OKEXTERN;

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

void sumStride (char * ARRAY a, int stride, char * ARRAY b) {
  foreach (i, 0, THREADS)
    for (int j = i; j < stride; j += THREADS) {
        a[j] = a[j + stride];
        accumulate (b, j);
    }
  endfor
}

int sum2 (char * ARRAY START NONNULL SIZE(len) a, int REF(V > 0) len)
  CHECK_TYPE
{
  char *log = (char *) malloc (len);
  for (int stride = len / 2; stride > 0; stride /= 2)
    sumStride (a, stride, log);

  return a[0];
}
