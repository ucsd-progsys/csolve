#include <string.h>

#ifndef DEPUTY
#define NTS
#define NT
#define NONNULL
#endif

/* A dumb way to search for a pattern in a string */
int substr(const char * NONNULL NTS pattern,
           const char * NONNULL NTS haystack) {
  const char * ph = haystack;
  while(*ph) {
    const char * pht = ph;
    const char * pp = pattern;
    while(*pp && *pht && *pp == *pht) {
      pp ++; pht ++;
    }
    if(! *pp) return 1; // We found the pattern

    ph ++;
  }
  return 0;
}

char (NT str)[1000 * 1000];
#define NR_ITER 3

int main() {
  int i;
  
  // Fill the string with a's
  memset(str, 'a', sizeof(str) - 1);
  str[sizeof(str) - 1] = '\0';

  for(i=0;i<NR_ITER;i++) {
    if(substr("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",
              str)) {
      return 1;
    }
  }
  return 0;
}
