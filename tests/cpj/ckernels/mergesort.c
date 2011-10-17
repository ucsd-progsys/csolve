#include <cpj.h>
#include <stdlib.h>
#include <math.h>

//void merge(int * a, int * b, int * lena, int lenb, int * c);
//bool check_sorted(int * buf, int len);
//int find_split(int m, int * b);
//void sort(int * a, int * b, int len);


const int BUF_LEN; //{v: int | v > 0}
const int MAX_INT; //{v: int | v > 0 && v > BUF_LEN}
const int MERGE_SIZE;

int main(char ** argv, int argc)
{
  int * in, out;
  int len;  
  
  len = BUF_LEN; 

  in  = malloc(sizeof(int)*len);
  out = malloc(sizeof(int)*len);

  sort(in, 0, len, out);
  assert(check_sorted(in));
}

void initialize(int * buf, int len) {
  int i;
  for(i = 0; i < len; i++)
    buf[i] = rnd(MAX_INT);
}

//a: ptr(l1, i1) / l1 => (0+: int)  -> b: ptr(l2, i2) / l2 => (0+: int)
//                                  -> len: int
//                                  -> () / h: l1 => (0+: int), l2 => (0+: int);
//                                          r: l1 => (i1 <= v < i1 + len), l2 => (i2 <= v < i2 + len);
//                                          w: l1 => (i1 <= v < i1 + len), l2 => (i2 <= v < i2 + len) 
void sort(int * a, int * b, int len) {
  int q  = len / 4;
  int h  = 2*q;
  int r  = len - 3*q;
   
  cobegin
    rtn(sort(a, b, q))
    rtn(sort(a + q, b + q, q))
    rtn(sort(a + h, b + h, q))
    rtn(sort(a + 3*q, len, r))
  coend 

  cobegin
    rtn(merge(a, a + q, q, q, b))
    rtn(merge(a + h, a + 3*q, q, r, b + h))
  coend 

  merge(b, b + h, h, len - h, a);
}

void seq_merge(int * a, int * b, int * lena, int lenb, int * c) 
{
  int i, j, k;
  i = j = k = 0;

  while (i < lena && j < lenb)
    if (a[i] < b[j])   
      c[k++] = a[i++];
    else
      c[k++] = b[j++];

  while (i < lena) c[k++] = a[i++];
  while (j < lenb) c[k++] = b[i++];
}

//a: ptr(l1, i1) / l1 => (0+: int) -> b: ptr(l2, i2) / l2 => (0+: int)
//                                 -> lena: int -> lenb: int
//                                 -> c: ptr(l3, i3) / l3 => (0+: int)
//                                 -> () / h: l1 => (0+: int), l2 => (0+: int), l3 => (0+: int);
//                                         r: l1 => (i1 <= v < i1 + lena), l2 => (i2 <= v < i2 + lenb), l3 => F
//                                         w: l1 => F, l2 => F, l3 => (i3 <= v < i3 + lena + lenb)
void merge(int * a, int * b, int lena, int lenb, int * c)
{
  if (lena <= MERGE_SIZE) 
    seq_merge(a, b, lena, lenb, c);
  else {
    int ha = a / 2;
    int sb = find_split(a[ha], b);
  }

  cobegin
    rtn(merge(a, b, ha, sb, c))
    rtn(merge(a + ha, b + sb, lena - ha, lenb - sb, c + ha + sb))
  coend
}

int find_split(int v, int * b, int len)
{
  int lo = 0;
  int hi = len;
  int m;

  while (lo < hi) {
    m = lo + abs(hi - lo) / 2;
    if (v <= b[m]) 
      hi = m;
    else
      lo = m + 1;
  }
  return hi;
}

bool check_sorted(int * buf, int len)
{
  int i; 
  for(i = 0; i < len - 1; i++)
    if(buf[i] <= buf[i + 1])
      return false;
  return true;
}
