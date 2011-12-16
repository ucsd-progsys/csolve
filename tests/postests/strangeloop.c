#include <csolve.h>

void main ()
{
  int n4 = nondet();

  if (n4 >= 1)
  {
    for (;;)
    {
      csolve_assert (n4 >= 0);
      if (--n4 == 0)
        return;
    }
  }
  return;
}
