#include <csolve.h>
#include <stdlib.h>

struct s {
  int x;
  char *y;
};

void register_cb(void * LOC(L) p,
                 void (* START VALIDPTR cb)(void * LOC(L)))
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
