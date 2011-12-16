#include <stdlib.h>
#include <csolve.h>

char * ARRAY LOC(D) strncpy (char * ARRAY LOC(D) dest, const char * ARRAY src, unsigned int n)
{
  char c;
  char *s = dest;

  unsigned int n4 = n / 4;	// JHALA
  unsigned int m4 = n % 4;	// JHALA

  //modular arith axiom: (n = (4 * n4) + m4), faked by assume
  if (n != (4 * n4) + m4) { STUCK: goto STUCK;}

  if (n4 >= 1) // n >= 4
    {
      //unsigned int n4 = n / 4;// n >> 2;

      for (;;)
	{
	  c = *src++;
	  *dest++ = c;
	  
	  if (c == 0)
	    break;
	  c = *src++;
	  *dest++ = c;
	  
	  if (c == 0)
	    break;
	  c = *src++;
	  *dest++ = c;
	  
	  if (c == 0)
	    break;
	  c = *src++;
	  *dest++ = c;
	  
	  if (c == 0)
	    break;
	  if (--n4 == 0)
	    goto last_chars;
	}
      n -= dest - s;
      goto zero_fill;
    }

last_chars:
  n = m4; // n &= 3; 
  if (n == 0)
    return dest;

  for (;;)
    {
      c = *src++;
      --n;
      *dest++ = c;
      if (c == 0)
	break;
      if (n == 0)
	return dest;
    }

 zero_fill:
  while (n-- > 0) {
    dest[n] = 0;
  }

  return dest - 1;
}

void main () {
    char s1[10];
    char s2[10];
    int  off;

    off = nondetpos();
    if (off < 10) {
        s1[off] = off;
        s2[off] = off;
    }

    strncpy (s1, s2, 5);
}
