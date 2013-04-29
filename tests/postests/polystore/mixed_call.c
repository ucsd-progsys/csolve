#include <csolve.h>
#include <stdlib.h>
#include <string.h>

struct s {
  /* int x; */
  char * ARRAY LOC(STR) y;
};

void register_cb(char * ARRAY LOC(STR) str,
		 void * VAR(a) p,
                 void (* START VALIDPTR cb)(char * ARRAY LOC(STR) x, void * VAR(a) y))
                 /* struct s INST(STR, STR) * LOC(P) p, */
                 /* void (* START VALIDPTR cb)(char * ARRAY LOC(STR) x, struct s INST(STR,STR) * LOC(P) OK MPTR y)) */
  /* OKEXTERN; */
{
  cb(str,p);
}

/* When the new store is subd it needs to be well formed wrt the new function */
void process_s(char * ARRAY LOC(X) p_s, struct s INST(STR, X) * OK MPTR p_p)
{
  validptr(p_p->y);
  /* if (strcmp(p_s, p_p->y)) */
  /* { */
  /*   p_p->x++; */
  /* } */
}
  

void main()
{
  struct s s1;
  s1.y = "asdf";//nondet() ? "foox" : "bary";
  register_cb("foo",&s1,process_s);
}
