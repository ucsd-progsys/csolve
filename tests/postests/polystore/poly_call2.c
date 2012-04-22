#include <csolve.h>
#include <stdlib.h>

struct s {
  int x;
  char *y;
};

void register_cb(void * VAR(a) p,
                 void (* START VALIDPTR cb)(void * VAR(a)))
{
  cb(p);
}

void process_s(struct s *p)
{
  if (p) {
    p->x = 1;
    p->y = "one";
  }
}

void main()
{
  struct s s1;
  register_cb(&s1, process_s);
}
