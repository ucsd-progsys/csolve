#include <cpj.h>
#include <stdlib.h>

//a: ptr(l, i) -> len: int -> () / h: l => (0+: int);
//                                 r: l => F;
//                                 w: l => i <= v < i + len;
void initialize(int * ARRAY a, int len) {
  int i;

  foreach(i, 0, len)
    a[i] = i;
  endfor 
}

//a: ptr(l, i) -> len: int -> seqLen: int -> int / h: l => (0+: int);
//                                                 r: l => i <= v < i + len;
//                                                 w: l => F
int reduce(int * ARRAY a, int len, int seqLen)
{ 
  int i = 0;

  if (len == 0) return 0;
  if (len == 1) return a[0];
  int result = 0;
  if (len > seqLen) {
    int tmp1, tmp2, h = len/2;

    cobegin
        rtn(tmp1 = reduce(a, h, seqLen))
        rtn(tmp2 = reduce(a + h, len - h, seqLen))
    coend 

    result = tmp1 + tmp2;
    }
    else
      for (i = 0; i < len; i++)
        result = result + a[i];
    return result;
}

int main( int REF(V > 0) argc
        , char * STRINGPTR * START NONNULL ARRAY SIZE(argc * 4) args) CHECK_TYPE
{
  
  
  
  if (argc != 3) return 1;
  
  int seqLen = atoi(args[0]);

  int * ARRAY arr;
  
  int len = nondetpos();

  arr = malloc(sizeof(int) * len);

  initialize(arr, len);
  reduce(arr, len, seqLen);

  return 0;
}




