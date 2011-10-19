#include <cpj.h>
#include <stdlib.h>

const int REF (V > 0) buf_len; 
const int REF (V > 0) too_small;


void swp(int * ARRAY a, int b, int c)
{
  int t = a[b];
  a[b] = a[c];
  a[c] = t;
}

//a: ptr(l, i) / l => (0+: int) -> len: int -> () / h: l => (0+; int)
//                                                  r: l => T
//                                                  w: l => T
void quicksort(int * ARRAY a, int len)
{
  int end = len - 1;
  int i, j, t;

  //do a sequential sort if array becomes too small
  if (len <= too_small)
  {
    for (i = 1; i <= end; i++)
    {
      t = a[i];
      j = i - 1;
      while (j >= 0 && a[j] > t)
      {
        a[j + 1] = a[j];
        j--;
      }
      a[j + 1] = t;
    }
    return;
  }

  //choose the median of the middle, beginning and end as the pivot
  //swap them into relative order while we're at it
  int mid = end / 2;

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

  while(1) {
    while(a[rt] > a[pt]) rt--;
    while(lt < rt && a[lt] <= a[pt]) lt++;

    if (lt < rt)
      swp(a, lt, rt--);
    else break; 
  }

  cobegin
    rtn(quicksort(a, lt + 1))
    rtn(quicksort(a + lt + 1, len - lt))
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


