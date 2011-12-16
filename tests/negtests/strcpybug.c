#include <csolve.h>

void test (const char *__attribute__((array)) src)
{
  char c;
  c = *src;
  // pmr: TOTALLY BOGUS
  csolve_assert (c == 0);
}

void main () {
    char s1[10];
    int  off;

    off = nondetpos();
    if (off < 10) {
        s1[off] = off;
    }

    test (s1);
}
