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

void initialize(int * ARRAY a, int len)
{ 
  foreach(i, 0, len)
    a[i] = nondet();
  endfor
}

void main() // char ** argv, int argc)
{
  int * buf;
  int len = buf_len;

  buf = malloc(sizeof(int) * buf_len); 

  initialize(buf, len); 
  quicksort(buf, len);
}

//a: ptr(l, i) / l => (0+: int) -> len: int -> () / h: l => (0+; int)
//                                                  r: l => T
//                                                  w: l => T
void quicksort(int * ARRAY a, int len)
{
  int end = len - 1;
  int i, j, t;

  if (hi - lo + 1 <= too_small) 
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

  int mid = end / 2;

  if (a[0] > a[mid])
    swp(0, mid);
  if (a[mid] > a[end])
  {
    swp(mid, end);
    if (a[0] > a[mid])
      swp(0, mid);
  }

  int lt = 1;
  int rt = end - 1;
  int pt = a[mid];

  while(1) {
    while(a[rt] > pt) rt--;
    while(lt < rt && a[lt] <= pt) lt++;

    if (lt < rt)
      swp(a[lt], a[rt--]);
    else break; 
  }

  cobegin
    rtn(quicksort(a + lo, a + lt + 1))
    rtn(quicksort(a + lt + 1, a + hi - lt))
  coend
}
