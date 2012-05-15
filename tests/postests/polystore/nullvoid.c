#include <csolve.h>

struct s {
  void * REF(true) d;
};

void mutate_s(struct s *p) CHECK_TYPE
{
    p->d = 0;
}
