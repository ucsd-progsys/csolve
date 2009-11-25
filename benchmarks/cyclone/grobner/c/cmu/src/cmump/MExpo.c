#include "cmump.h"
#include "multpol.h"

int expoequal(short int *exp1, short int *exp2)
{
	register i;

	if(exp1[0]!=exp2[0])
		return(0);
	for(i=1;i<=nvars;i++)
		if(exp1[i]!=exp2[i])
			return(0);
	return(1);
}


#if 0
void expocopy(exp1,exp2) short exp1[],exp2[];
{
	register short *p1;
	register i;
	for(i=0;i<=nvars;i++)
		exp2[i] = exp1[i];
}
#endif

void expocopy (short int *exp1, short int *exp2)
{
	bcopy ( exp1, exp2, (nvars+1)*sizeof(short) );
}


int expozero(short int *exp)
{
	return((exp[0]==0) ? 1 : 0);
}


void exposub(short int *exp1, short int *exp2, short int *exp3)
{
  register short *p1,*p2;
  register i;

  p1=exp1;
  p2=exp2;
  for (i=0;i<=nvars;i++) exp3[i]=(*(p1++)-(*(p2++)));
}



void expoadd(short int *exp1, short int *exp2, short int *exp3)
{
  register short *p1,*p2;
  register i;

  p1=exp1;
  p2=exp2;
  for (i=0;i<=nvars;i++) exp3[i]=(*(p1++)+(*(p2++)));
};


void expomax(short int *exp1, short int *exp2, short int *exp3)
{
  register i;

  exp3[0]=0;
  for (i=1;i<=nvars;i++) 
	exp3[0] += (exp3[i]= (exp1[i]>exp2[i]) ? exp1[i] : exp2[i]);
};


/* expodiv : returns 1 if exp1[i] <= exp2[i] for all i */
#if 0
int expodiv(exp1,exp2) short exp1[],exp2[];
{
  register i;

  for(i=0;i<=nvars;i++)
    if(exp1[i]>exp2[i])
      return(0);
  return(1);
}
#endif

int expodiv (register short int *exp1, register short int *exp2)
{
	register short *last = exp1 + (int)(nvars+1);
	while ( exp1 < last )
		if ( *(exp1++) > *(exp2++) )
			return 0;
	return 1;
}



/* expostrictdiv : returns 1 if exp1[i] <= exp2[i] for all i 
and exp1[i] < exp2[i] for at least one i. */

int expostrictdiv(short int *exp1, short int *exp2)
{
  register i;

  if (exp1[0]>=exp2[0]) return(0);
  for (i=1;(i<=nvars) && (exp1[i]<=exp2[i]);i++);
  return (i==(nvars+1)) ? 1 : 0;
};


/* expofactor : find exp3 such that exp1 + exp3 = sup(exp1,exp2) */

void expofactor(short int *exp1, short int *exp2, short int *exp3)
{
  register i;

  exp3[0]=0;  
  for (i=1;i<=nvars;i++) {
	exp3[i]= exp2[i]-exp1[i];
	if (exp3[i]<0) exp3[i]=0;
	exp3[0]+=exp3[i];
  }
};


/* tests if the two power products do not have any factor in common */

int expocrit2(short int *exp1, short int *exp2)
{
  register i;

  for (i=1;(i<=nvars) && ((exp1[i]==0)||(exp2[i]==0));i++);
  return (i==(nvars+1));
};



/* Now come all the different orders. One or the other will be chosen, depending on
** the value of the parameter order_exp. This will be done in the function order_init
** which must be called before any use of this package.
*/


int cmp_lex_exp(short int *exp1, short int *exp2)                     

/* cmp_lex_exp -- integer function returning 1 (resp -1,0) if the exponent 
pointed to by the first pointer is greater than (resp less than, equal to) the 
exponent pointed to by the second pointer in the lexicographical ordering. */

{
  register i;

  for (i=1;(i<=nvars) && (exp1[i]==exp2[i]);i++);
  if (i==nvars+1) return(0);
  return((exp1[i]>=exp2[i])?1:-1);};




int cmp_td_exp(short int *exp1, short int *exp2)                     

/* cmp_td_exp -- integer function returning 1 (resp -1,0) if the exponent 
pointed to by the first pointer is greater than (resp less than, equal to) the 
exponent pointed to by the second pointer in the total order refined by the 
lexicographic order. */

{
  register i;

  for (i=0;(i<nvars+1) && (exp1[i]==exp2[i]);i++);
  if (i==nvars+1) return(0);
  return((exp1[i]>=exp2[i])?1:-1);};



int cmp_revlex_exp(short int *exp1, short int *exp2)                     

/* cmp_revlex_exp -- integer function returning 1 (resp -1,0) if the exponent 
pointed to by the first pointer is greater than (resp less than, equal to) the 
exponent pointed to by the second pointer in the total order refined by the 
reverse lexicographic order. */

{
  register i;

  if (exp1[0]!=exp2[0])
	return((exp1[0]>exp2[0])?1:-1);
  else {
  	for (i=nvars;(i>0) && (exp1[i]==exp2[i]);i--);
  if (i==0) return(0);
  return((exp1[i]<=exp2[i])?1:-1);};
};


int cmp_double_revlex_exp(short int *exp1, short int *exp2)                     

/* cmp_double_revlex_exp -- integer function returning 1 (resp -1,0) if the exponent 
pointed to by the first pointer is greater than (resp less than, equal to) the 
exponent pointed to by the second pointer in the following order : total order 
refined by the reverse lexicographic order on the first "first_group" variables 
refined by the same order on the other variables. */

{
  register i,f1,f2;

  f1=0;f2=0;
  for (i=1;i<first_group+1;i++){ f1 += exp1[i]; f2 += exp2[i];};
  if (f1!=f2)
	return((f1>f2)?1:-1);
  for (i=first_group+1;(i>0) && (exp1[i]==exp2[i]);i--);
  if (i!=0) return((exp1[i]<exp2[i]) ? 1 : -1);
  if ((exp1[0]-f1) != (exp2[0]-f2)) 
	return (((exp1[0]-f1)> (exp2[0]-f2))?1:-1);
  for (i=nvars;(i>first_group+1) && (exp1[i]==exp2[i]);i--);
  if (i==first_group+1) return(0);
  return ((exp1[i]<=exp2[i])?1:-1);
};







int (*cmp_exp)();

void init_order_exp(void)
{
	switch(order_exp){
	case 0 : cmp_exp = cmp_lex_exp;
		 break;
	case 1 : cmp_exp = cmp_td_exp;
		 break;
	case 2 : cmp_exp = cmp_revlex_exp;
		 break;
	case 3 : cmp_exp = cmp_double_revlex_exp;
		 break;
	};
};


