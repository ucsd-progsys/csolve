#include <csolve.h>

struct my_struct {
  void *VAR(a) a;
  void *VAR(b) b;
};

#define s struct my_struct INST_VAR(a,b) INST_VAR(b,b)
#define s_unsafe struct my_struct

void foo(s *p) CHECK_TYPE
{
  p->a = p->b; //SAFE!
}
