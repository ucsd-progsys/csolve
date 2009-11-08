#define d294 0

#include "cmump.h"
#include "multpol.h"
#include "gbas.h"
#include <stdio.h>

extern short nvars;
extern int (*cmp_exp)();
extern int spy, order_on_pairs,reduction_first;

/* --------------------------------------------------------------------------------------------- */

/*	pairs contains the list of pairs. They are listed in decreasing order w.r.t.
	new_cmp_pairs and choose_pair picks the lowest one.  */

new_choose_pair(polpairsettype *sp, MPOL **p1, MPOL **p2)
{
	 sp->npairs--;
	 (*p1) = sp->polpair[sp->npairs].p1;
	 (*p2) = sp->polpair[sp->npairs].p2;
	 if (sp->npairs==0)
		gfree(sp->polpair);
	return;
}

/* --------------------------------------------------------------------------------------------- */

/*	This function returns the new set of pairs "spair" between "p" and the 
	polynomials of "spol". It does not perform any filtration. 
	"scratch" provides some space to put partial results.  */

void new_build_new_pairs(polsettype *spol, MPOL *p, polpairsettype *spair, short int *scratch)
{
	register j;
	register MPOL *pt;

	if(spair->npairs>0)
		gfree(spair->polpair);
	spair->polpair = (polpairtype*)galloc(spol->npols*sizeof(polpairtype));

	pt=spol->polp;

	while(pt==p)
		pt=pt->next;
	spair->polpair[0].p1 = pt;
	spair->polpair[0].p2 = p;
	spair->npairs = 1;
	pt=pt->next;

	while(pt!=NULL){
		if(pt!=p){
			lpp_lbd_pair(pt,p,scratch);
			lpp_lbd_pair(spair->polpair[spair->npairs-1].p1,
				spair->polpair[spair->npairs-1].p2,scratch+nvars+1);
			j=spair->npairs-1;

			while ((j>=0)&&((*cmp_exp)(scratch,scratch+nvars+1)>=0)){
				spair->polpair[j+1]=spair->polpair[j];
				j--;
				if (j>=0)
					lpp_lbd_pair(spair->polpair[j].p1,
						spair->polpair[j].p2, scratch+nvars+1);
			}
			j++;
			spair->polpair[j].p1=pt;
			spair->polpair[j].p2=p;
			spair->npairs++;
		}
		pt = pt->next;
	}
	return;
}

/* --------------------------------------------------------------------------------------------- */

void new_order_cmp_pair(polpairsettype *sp)
{
	polpairsettype temp;
	register i,j;

	if(sp->npairs==0)
		return;
	temp.polpair = (polpairtype*)galloc(sp->npairs*sizeof(polpairtype));

	temp.polpair[0] = sp->polpair[0];
	temp.npairs = 1;

	for (i=1;i<sp->npairs;i++){
		j=temp.npairs-1;

		while ((j>=0)&& new_cmp_pairs(&(sp->polpair[i]),&(temp.polpair[j]))>=0){
			temp.polpair[j+1]=temp.polpair[j];
			j--;
		}
		j++;
		temp.polpair[j] = sp->polpair[i];
		temp.npairs++;
	}
	gfree(sp->polpair);
	sp->polpair = temp.polpair;
	return;
}


/* --------------------------------------------------------------------------------------------- */

int new_cmp_pairs(polpairtype *pa1, polpairtype *pa2)
{	
  static short *exp1, *exp2;

  if (!exp1)
    {
      exp1 = (short *)gstralloc(MAXVARS * sizeof(short));
      exp2 = (short *)gstralloc(MAXVARS * sizeof(short));
    }

	/*
	exp1 = (short*)galloc(2*(int)(nvars+1));
	exp2 = (short*)galloc(2*(int)(nvars+1));
	*/
	lpp_lbd_pair(pa1->p1,pa1->p2,exp1);
	lpp_lbd_pair(pa2->p1,pa2->p2,exp2);
	if (reduction_first){
		if(expodiv(MEXPO(pa1->p1,0), MEXPO(pa1->p2,0))||
			(expodiv(MEXPO(pa1->p2,0), MEXPO(pa1->p1,0))))
			if(expodiv(MEXPO(pa2->p1,0), MEXPO(pa2->p2,0))||
				(expodiv(MEXPO(pa2->p2,0), MEXPO(pa2->p1,0))))
				goto yuck;
		else 
			return(-1);

		if(expodiv(MEXPO(pa2->p1,0), MEXPO(pa2->p2,0))||
			(expodiv(MEXPO(pa2->p2,0), MEXPO(pa2->p1,0))))
			return(1);
	}
yuck:   switch(order_on_pairs){
		case 0: 
			if (exp1[0] != exp2[0]) 
				return (exp1[0] - exp2[0]);
			else 
				return (cmp_exp(exp1,exp2));
			break;
		case 1: 
			if (exp1[0] != exp2[0]) 
				return (exp2[0] - exp1[0]);
			else 
				return (cmp_exp(exp2,exp1));
			break;
		case 2:
			return (cmp_exp(exp1,exp2));
			break;
		case 3:
			return (cmp_exp(exp2,exp1));
			break;
	}
}

/* --------------------------------------------------------------------------------------------- */

/*	Delete in the set of pairs "spair", the pairs that won't need to be reduced
	due to the apparition of polynomial "p" in the basis. "scratch" is just a
	pointer to a dynamically allocated array.  */

void new_update_old_pairs(polpairsettype *spair, MPOL *p, short int *scratch)
{	
	register pos,i;
	int new_npairs;

	pos=0; new_npairs=spair->npairs;
	for (i=0;i<spair->npairs;i++){
		lpp_lbd_pair(spair->polpair[i].p1,p,scratch);
		lpp_lbd_pair(spair->polpair[i].p2,p,scratch+nvars+1);
		lpp_lbd_pair(spair->polpair[i].p1,
			spair->polpair[i].p2,scratch+2*(nvars+1));
		if(expostrictdiv(scratch,scratch+2*(nvars+1))&&
			expostrictdiv(scratch+nvars+1,scratch+2*(nvars+1))){
			new_npairs--;
			/* beep(); */
		}
		else {
			if(i!=pos)
				spair->polpair[pos]=spair->polpair[i];
			pos++;
		}
	}
	if((new_npairs==0)&&(spair->npairs!=0))
		gfree((char *)spair->polpair);
	spair->npairs=new_npairs;
	return;
}

/* --------------------------------------------------------------------------------------------- */

/*	keep only one pair from all the pairs with same lpp, the one satisfying
	Buchberger criterium 2, if there exists one.  */

void new_filter_same_lpp(polpairsettype *spair, short int *scratch)
{	
	register i,j,flag;

	j=0;
	flag = expocrit2(MEXPO(spair->polpair[0].p1,0), MEXPO(spair->polpair[0].p2,0));
	lpp_lbd_pair(spair->polpair[0].p1,spair->polpair[0].p2,scratch);
	for(i=1;i<spair->npairs;i++){
		lpp_lbd_pair(spair->polpair[i].p1, spair->polpair[i].p2,scratch+nvars+1);
		if(expoequal(scratch,scratch+nvars+1)){
			if((!flag)&&(expocrit2(MEXPO(spair->polpair[i].p1,0),
				MEXPO(spair->polpair[i].p2,0)))){
				flag=1;
				spair->polpair[j]=spair->polpair[i];
			}
		}
		else{
			j++;
			spair->polpair[j]=spair->polpair[i];
			flag=expocrit2(MEXPO(spair->polpair[i].p1,0),MEXPO(spair->polpair[i].p2,0));
			expocopy(scratch+nvars+1,scratch);
		}
	}
	spair->npairs=j+1;
	return;
}

/* --------------------------------------------------------------------------------------------- */

/*	update pairset to include all pairs formed due to inclusion of p in spol.
	p DOES belong to spol, so call this after updating the basis itself  */

void new_update_pairs(polpairsettype *spair, polsettype *spol, MPOL *p)
{
	register i,j,k;
	short *scratch;
	polpairsettype temp,final;

	if (spol->npols==0)
		return;
	scratch = (short *)gstralloc(sizeof(short)*6*(int)(nvars+1));

	/* First, delete in the initial set of pairs those who disappear because
	of criterium1 (with the pairs brought by the new polynomial ) */

	new_update_old_pairs(spair,p,scratch);
	 
	/* Now we build the set of new pairs */
	temp.npairs = 0;
	new_build_new_pairs(spol,p,&temp,scratch);

	/* Now in each group of the set of new pairs having the same lpp_pair, we keep
	only one. We keep the one satisfying Buchberger criterium 2, if any. Flag is 
	set to 1 when a pair satisfying crit2 is found */

	new_filter_same_lpp(&temp,scratch);

	/* now, we want to eliminate from the set of new pairs the pairs whose lpp 
	dominates the lpp of another pair in the set. In the same time, we eliminate 
	pairs satisfying crit2. BE CAREFUL : the order in which the pairs are 
	inspected is essential. We also update the field "np"  */

	/* now, we order the set of new pairs according to new_cmp_pair */
	new_order_cmp_pair(&temp);

	/* and we need to regroup spair and temp in final. */

	final.npairs = spair->npairs + temp.npairs;
	if (final.npairs!=0)
		final.polpair = (polpairtype*)galloc(final.npairs*sizeof(polpairtype));
	i=0;j=0;k=0;
	/* merge */
	while((i<spair->npairs)&&(j<temp.npairs))
		if (new_cmp_pairs(&(spair->polpair[i]),&(temp.polpair[j]))>=0){
			final.polpair[k]=spair->polpair[i];i++;k++;
		}
		else{
			final.polpair[k]=temp.polpair[j];j++;k++;
		}
	while(i<spair->npairs){
		final.polpair[k]=spair->polpair[i];
		i++;k++;
	}
	while(j<temp.npairs){
		final.polpair[k]=temp.polpair[j];
		j++;k++;
	}
	
	gfree(temp.polpair);
	if(spair->npairs>0)
		gfree(spair->polpair);
	gfree((char *)scratch);
	(*spair)=final;
	return;
}

