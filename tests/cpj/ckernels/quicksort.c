#include <cpj.h>
#include <stdlib.h>

const int BUF_LEN;
const int TOO_SMALL;
const int MAX_INT;

void swp(int * a, int b, int c)
{
  int t = a[b];
  a[b] = a[c];
  a[c] = t;
}

void initialize(int * a, int len)
{ 
  foreach(i, 0, len)
    a[i] = nondet();
  endfor
}

int main(char ** argv, int argc)
{
  int * buf;
  int len = BUF_LEN;

  buf = malloc(sizeof(int) * BUF_LEN); 

  initialize(buf, len); 
  quicksort(buf, len);
}

//a: ptr(l, i) / l => (0+: int) -> len: int -> () / h: l => (0+; int)
//                                                  r: l => T
//                                                  w: l => T
void quicksort(int * a, int len)
{
  int end = len - 1;
  int i, j, t;

  if (hi - lo + 1 <= TOO_SMALL) 
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
