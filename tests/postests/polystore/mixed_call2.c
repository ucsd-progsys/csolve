#include <csolve.h>
#include <stdlib.h>
#include <string.h>

struct s {
  int x;
  char * ARRAY LOC(STR) y;
};

struct big_s {
  int padding;
  char * ARRAY LOC(FOO) big_y;
  int big_x;
};

void register_cb(char * ARRAY LOC(STR) str,
		 void * VAR(a) p,
                 void (* START VALIDPTR cb)(char * ARRAY LOC(STR), void * VAR(a)))
{
  cb(str, p);
}

void process(char * ARRAY LOC(STR) str, struct s INST(STR, STR) *p)
{
  if (strcmp(str, p->y))
  {
    p->x++;
  }
}

void process_big(char *ARRAY LOC(FOO) str, struct big_s INST(FOO, FOO) *p)
{
  if (strcmp(str, p->big_y))
  {
    p->big_x++;
  }
}
  

void main()
{
  struct s s1;
  struct big_s big_s1;
  
  s1.y = nondet() ? "foo" : "bar";
  big_s1.big_y = nondet() ? "big_foo" : "big_bar";
  
  register_cb("foo", &s1, process);
  register_cb("foo", &big_s1, process_big);
}
