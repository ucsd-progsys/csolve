#include <stdlib.h>
#include <csolve.h>
#include <cpj.h>

#define MAX_SPLIT_DEPTH 8

// Show: commutative atomic effect; user data effect

CSOLVE_EFFECT (EAtomic)
CSOLVE_EFFECTS_COMMUTE (EAtomic, EAtomic)
CSOLVE_EFFECTS_COMMUTE (EAtomic, EREAD)

extern void increment (int * LOC(L) p)
  EFFECT(L, && [EAtomic = 1; EWRITE != 1; EREAD != 1])
  OKEXTERN;

extern void decrement (int * LOC(L) p)
  EFFECT(L, && [EAtomic = 1; EWRITE != 1; EREAD != 1])
  OKEXTERN;

// CSOLVE_EFFECT(EUserData)
// But how to enforce that a function does not have user data effect? With CHECK_TYPE?

void
interleave(int len,
	   char * STRINGPTR stream1,
	   char * STRINGPTR stream2,
	   char * STRINGPTR out)
{
  int i;

  cobegin
   rtbeg
    for (i = 0; i < len; i++)
      out[i * 2] = stream1[i];
   rtend
   rtbeg
     for (i = 0; i < len; i++)
       out[i * 2 + 1] = stream2[i];
   rtend
  coend
}

void project (int len,
              char * STRINGPTR in,
              char mask,
              char * STRINGPTR out)
{
    for (int i = 0; i < len; i) {
        out[i] = in[i] & mask;
    }
}

void split (int *splitLevels,
            int len,
            char * STRINGPTR in,
            char * STRINGPTR out1,
            char * STRINGPTR out2)
{
    if (len < 2 || *splitLevels >= MAX_SPLIT_DEPTH) {
        cobegin
            rtn (project (len, in, 0xF0, out1))
            rtn (project (len, in, 0x0F, out2))
        coend

        return;
    }

    int hl = len / 2;

    increment (splitLevels);
    cobegin
        rtn (split (splitLevels, hl, in, out1, out2))
        rtn (split (splitLevels, len - hl, in + hl, out1 + hl, out2 + hl))
    coend
    decrement (splitLevels);
}

void main () {
  // Advanced: read length of data from user and "scrub"?
  int   len  = nondetpos ();
  char *in   = (char *) malloc (len);
  char *out1 = (char *) malloc (len);
  char *out2 = (char *) malloc (len);
  char *out3 = (char *) malloc (2 * len);
  int sl     = 0;

  foreach (i, 0, len)
    in[i] = nondet ();
  endfor

  split (&sl, len, in, out1, out2);
  interleave (len, out1, out2, out3);
}
