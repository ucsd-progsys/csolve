#include <csolve.h>

typedef void (*fptr)(void);

void register_cb(fptr f) OKEXTERN;
void foo(void) OKEXTERN;

int main()
{
  register_cb(&foo);

  return 0;
}
