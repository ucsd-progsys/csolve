#include <csolve.h>
#include <stdlib.h>

struct s {
  int x;
  char *y;
};

void register_cb(void * VAR(L) p,
                 void (* START VALIDPTR cb)(void * VAR(L)))
{
  cb(p); 
}

void process_s(struct s *p)
{
  p->x = 1;
  p->y = "one";
}

void main()
{
  struct s *s1 = malloc(sizeof(*s1));
  struct s *s  = nondet() ? s1 : NULL;
 
  // UNSAFE 
  register_cb(s, process_s);
}
