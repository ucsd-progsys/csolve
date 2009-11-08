#include <stdio.h>
#include "cmump.h"
#include "multpol.h"


#define NBUF 1000

extern short nvars;
extern char *varnames[];

/* I don't know how to use yacc in a less ugly way */

static MPOL tempmon,temp;
static MINT coef;
static short expo[1000];
static int status;
static char *numbers[NBUF];

#include "y.tab.c"

/* returns 0 if the input is correct
	   1 if the input is empty
           2 if there is a syntax error
	   3 if there is an unknown 
	   4 if the buffer is too small */

#ifdef YYDEBUG
extern int yydebug;
#endif

int mpolin(MPOL *p)
{
  MPOLINIT(&tempmon);
  MPOLINIT(&temp);
  MINIT(&coef);
  status = 0;
  initall();

#ifdef YYDEBUG
  yydebug=1;
#endif
  yyparse();
  mpolfree(&tempmon);
  if (status==0)
    MPOLMOVEFREE(&temp,p);
  else{
    mpolfree(&temp);
    mpolfree(p);
  };
  clean_num();
  return(status);
};


clean1(void)
{
  int i;
  for (i=0;i<=nvars;i++) expo[i]=0;
}

initall(void)
{
  int i;

  clean1();
  for (i=0;i<NBUF;i++)numbers[i]=NULL;
}

clean_num(void)
{
	register i;

	for (i=0;i<NBUF;i++)
		if (numbers[i]!=NULL) {
			free((char *)numbers[i]);
			numbers[i]=NULL;
		}
}

