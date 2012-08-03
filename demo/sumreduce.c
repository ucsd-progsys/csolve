// The following program sums the contents of an array by summing both
// halves in parallel, unless the length of the array is within some
// lower bound, in which case it is summed sequentially.
//
// Note that the array is initialized in parallel.
//
// CSolve proves all memory accesses safe and that the program behaves
// deterministically when executed in parallel.

#include <stdlib.h>
#include <cpj.h>

void initialize(int * ARRAY a, int len) {
  int i;

  foreach(i, 0, len)
    a[i] = i;
  endfor
}

int reduce(int * ARRAY a, int len, int seqLen) {
  int i = 0;

  if (len == 0) return 0;
  if (len == 1) return a[0];

  int result = 0;
  if (len > seqLen) {
    int tmp1, tmp2, h = len/2;

    cobegin
      rtn(tmp1 = reduce(a, h, seqLen));
      rtn(tmp2 = reduce(a + h, len - h, seqLen));
    coend

    result = tmp1 + tmp2;
  } else {
    for (i = 0; i < len; i++) {
      result = result + a[i];
    }
  }

  return result;
}

int main()
{
  int seqLen = nondetpos ();
  int len    = nondetpos ();
  int *arr   = malloc (sizeof(int) * len);
  initialize (arr, len);
  reduce (arr, len, seqLen);

  return 0;
}
