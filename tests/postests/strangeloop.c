#include <liquidc.h>

void main ()
{
  int n4 = nondet();

  if (n4 >= 1)
  {
    for (;;)
    {
      assert (n4 >= 0);
      if (--n4 == 0)
        return;
    }
  }
  return;
}
