#include <csolve.h>
#include <stdlib.h>
#include <string.h>

struct s {
  int x;
  char * ARRAY LOC(STR) y;
};

void register_cb(char * ARRAY LOC(STR) str,
		 void * VAR(a) p,
                 void (* START VALIDPTR cb)(char * ARRAY LOC(STR), void * VAR(a)))
{
  cb(str, p); 
}

void process_s(char * ARRAY LOC(STR) str, struct s INST(STR, STR) *p)
{
  if (strcmp(str, p->y))
  {
    p->x++;
  }
}
  

void main()
{
  struct s s1;
  s1.y = nondet() ? "foo" : NULL;    // UNSAFE!
  register_cb("foo", &s1, process_s);
}
