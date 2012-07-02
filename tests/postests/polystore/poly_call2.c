#include <csolve.h>
#include <stdlib.h>

#define csolvestr char NULLTERMSTR *ARRAY STRINGPTR

struct s {
  int x;
  csolvestr y;
};

void register_cb(void * VAR(a) p,
                 void (* START VALIDPTR cb)(void * VAR(a)))
{
  cb(p);
}

void process_s(struct s * LOC(L) s)
{
  if (s) {
    s->x = 1;
    s->y = "one";
  } 
}

void main()
{
  struct s s1;
  if (nondet()) {
    register_cb(&s1, process_s);
  } else {
    register_cb(NULL, process_s);
  }
}
