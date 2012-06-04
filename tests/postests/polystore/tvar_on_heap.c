#include <csolve.h>

struct my_struct {
  void * VAR(a) x;
};

void foo(struct my_struct *s) CHECK_TYPE
{
  s->x = s->x;
}
