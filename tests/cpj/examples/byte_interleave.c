#include <stdlib.h>
#include <csolve.h>
#include <cpj.h>

void
interleave(int REF (V > 0) n,
	   char * STRINGPTR START SIZE(n) stream1,
	   char * STRINGPTR START SIZE(n) stream2,
	   char * STRINGPTR START SIZE(2*n) out)
  CHECK_TYPE
{
  int i;

  cobegin
   rtbeg
    for(i = 0; i < n; i++)
      out[i*2] = stream1[i];
   rtend
   rtbeg
     for(i = 0; i < n; i++)
       out[i*2+1] = stream2[i];
   rtend
  coend
}


