#include <cpj.h>
#include <stdlib.h>

const int REF (V > 0) buf_len = 10000;
const int REF (V > 0) too_small  = 50;


void swp(int * ARRAY a, int b, int c)
{
  int t = a[b];
  a[b] = a[c];
  a[c] = t;
}

void seqsort(int * ARRAY a, int len){
  for (int i = 1; i < len; i++){
    int t = a[i];
    int j = i - 1;
    while (j >= 0 && a[j] > t){
      a[j + 1] = a[j];
      j--;
    }
    a[j + 1] = t;
  }
  return;
}

//a: ptr(l, i) / l => (0+: int) -> len: int -> () / h: l => (0+; int)
//                                                  r: l => T
//                                                  w: l => T
void quicksort(int * ARRAY a, int len)
{
  int end = len - 1;
  int i, j, t;

  //do a sequential sort if array becomes too small
  if (len <= too_small) {
    seqsort(a, len);
    return;
  }

  int tmp = a[0];
  csolve_assert(len > 0);

  //choose the median of the middle, beginning and end as the pivot
  //swap them into relative order while we're at it
  int mid = end / 2;

  int lt = 1;

  if (a[0] > a[mid])
    swp(a, 0, mid);
  if (a[mid] > a[end])
  {
    swp(a, mid, end);
    if (a[0] > a[mid])
      swp(a, 0, mid);
  }


  int lt = 1;
  int rt = end - 1;
  int pt = mid;

  while(lt < rt) {
    while(lt < rt && a[pt] <  a[rt])
      rt--;
    while(lt < rt && a[lt] <= a[pt])
      lt++;
    if (lt < rt)
      swp(a, lt++, rt--);
  }

  lt++;

  // pmr: inserted off-by-one below
  cobegin
    rtn(quicksort(a, lt))
    rtn(quicksort(a + lt - 1, len - lt))
  coend

}

void initialize(int * ARRAY a, int len)
{
  foreach(i, 0, len)
    a[i] = nondet();
  endfor
}

void main() // char ** argv, int argc)
{
  int * ARRAY buf;
  int len = buf_len;

  buf = malloc(sizeof(int) * len);

  initialize(buf, len);
  quicksort(buf, len);
}
