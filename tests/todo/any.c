#include <stdlib.h>
#include <csolve.h>


int main(int ANY argc, char ** argv)
{
  argc = 5;
  csolve_assert(argc == 5);

  return argc;
}
