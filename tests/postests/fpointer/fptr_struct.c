#include <csolve.h>
#include <stdlib.h>

struct fun_struct {
  void (*fptr)(void * VAR(a));
};

void cmp(struct fun_struct *s) CHECK_TYPE
{
  int x = 0;
  
  if (s->fptr || x) {
  } else {
    csolve_assert(0);
  }
}
