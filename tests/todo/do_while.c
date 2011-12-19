#include <csolve.h>
void main()
{
  int r = 2;

  do {
    lcc_assert(r > 0);
  } while (--r > 1);
}

