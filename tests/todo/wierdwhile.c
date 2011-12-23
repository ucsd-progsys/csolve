#include <csolve.h>

void main()
{
  int x = 100;
  while (1) {
    csolve_assert (x >= 0);
    x--;
    if (x < 0) goto hell;
  }

hell: ;
}
