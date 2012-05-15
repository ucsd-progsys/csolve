#include <csolve.h>

struct my_struct {
  void * VAR(a) x;
};

void bar(struct my_struct *a, struct my_struct *b) CHECK_TYPE
{
  a->x = b->x;
}
