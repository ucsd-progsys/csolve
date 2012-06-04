#include <csolve.h>
#include <stdlib.h>

struct fun_struct {
  void (*fptr)(void * VAR(a));
};

void cmp(struct fun_struct *s) CHECK_TYPE
{
  if (s->fptr != NULL) {
  } else {
    csolve_assert(0);
  }
}
