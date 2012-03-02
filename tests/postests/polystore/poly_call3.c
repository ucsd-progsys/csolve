#include <csolve.h>
#include <stdlib.h>

struct s {
  int x;
  char * ARRAY y;
};

struct t {
  char * ARRAY one;
  char * ARRAY two;
  short z;
};

void register_cb(void * LOC(K) s,
		 void * LOC(L) t,
                 void (* START VALIDPTR cb)(void * LOC(K), void * LOC(L)))
{
  cb(s,t); 
}

extern void process_s(struct s *s, struct t *t) OKEXTERN;

void main()
{
  struct s s1;
  struct t t1;
  register_cb(&s1, &t1, process_s);
}
