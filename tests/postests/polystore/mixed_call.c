#include <csolve.h>
#include <stdlib.h>
#include <stdio.h>

struct s {
  int x;
  char *y;
};

void register_cb(char * ARRAY  LOC(STR) str,
		 void * LOC(L) p,
                 void (* START VALIDPTR cb)(char * ARRAY LOC(STR), void * LOC(L)))
{
  cb(str, p); 
}

void process_s(char * ARRAY str, struct s *p)
{
  printf(str);
  
  p->x = 1;
  p->y = "one!";
}

void main()
{
  struct s s1;
  register_cb("/foo/bar/", &s1, process_s);
}
