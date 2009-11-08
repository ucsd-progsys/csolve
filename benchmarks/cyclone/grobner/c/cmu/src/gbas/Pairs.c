#include "cmump.h"
#include "multpol.h"
#include "gbas.h"
#include <stdio.h>

#if	eager_kills
extern	LMPOL *current_pair[MAX_WORKERS][2];	/* Hold the pair */
extern  int kill_pair[MAX_WORKERS];	
extern  int valid_pair[MAX_WORKERS];
extern	nworkers,co_workers;
#endif	eager_kills

#if	full_kills
struct finished_pair *finished_pair_list = NULL;
#endif	full_kills

extern short nvars;
extern int (*cmp_exp)();
extern int spy, order_on_pairs,reduction_first;

/* pairs contains the list of pairs. They are listed in decreasing order w.r.t.
** cmp_pairs and choose_pair picks the lowest one.
*/

choose_pair(sp,p1,p2) PAIRSET *sp; LMPOL **p1,**p2;
{
	 (*p1) = sp->pairs[sp->npairs-1].p1;
	 (*p2) = sp->pairs[sp->npairs-1].p2;
	 sp->npairs--;
	 if (sp->npairs==0) free((char *)sp->pairs);

#if	eager_kills
	 valid_pair[ ME % nworkers ] = 0;
	 current_pair[ ME % nworkers ][0] = *p1;
	 current_pair[ ME % nworkers ][1] = *p2;
	 kill_pair[ ME % nworkers ] = 0;
	 valid_pair[ ME % nworkers ] = 1;
#endif	eager_kills
};

/* p DOES belong to spol */
update_pairs(spair,spol,p) PAIRSET *spair; MPOLSET *spol; LMPOL *p; 
#define d294 1
{
	register i,j,k;
	short *scratch;
	PAIRSET temp,final;

	if (spol->npols==0) return;
	if ((scratch = (short *)galloc(6*(int)(nvars+1)))==0)
		valerr();

#if	eager_kills
	kill_current_pairs(p,scratch);
#endif	eager_kills

#if	full_kills
	kill_finished_pairs(p,scratch);
#endif	full_kills

	/* First, delete in the initial set of pairs those who disappear because
	of criterium1 (with the pairs brought by the new polynomial ) */
#if d294
	update_old_pairs(spair,p,scratch);
#endif d294
	 
	/* Now we build the set of new pairs */
	build_new_pairs(spol,p,&temp,scratch);

	/* Now in each group of the set of new pairs having the same lpp_pair, we keep
	only one. We keep the one satisfying Buchberger criterium 2, if any. Flag is 
	set to 1 when a pair satisfying crit2 is found */
#if d294
	filter_same_lpp(&temp,scratch);

	/* now, we want to eliminate from the set of new pairs the pairs whose lpp 
	dominates the lpp of another pair in the set. In the same time, we eliminate 
	pairs satisfying crit2. BE CAREFUL : the order in which the pairs are 
	inspected is essential. We also update the field "np"  */

	filter_bigger_lpp_or_crit2(&temp,scratch);

	/* now, we order the set of new pairs according to cmp_pair */
	order_cmp_pair(&temp);

#if	full_kills
	if (valid_pair[ME]) {
	  add_to_finished_pairs(current_pair[ME][0],current_pair[ME][1],&temp);
	}
#endif	full_kills

	/* and we need to regroup spair and temp in final. */

#endif d294

	final.npairs = spair->npairs + temp.npairs;
	if (final.npairs!=0)
		if ((final.pairs = (POLPAIR *)
		      galloc((unsigned)(final.npairs*sizeof(POLPAIR))))==0)
		    valerr();
	i=0;j=0;k=0;

	while((i<spair->npairs)&&(j<temp.npairs))
		if (cmp_pairs(&(spair->pairs[i]),&(temp.pairs[j]))>=0){
			final.pairs[k]=spair->pairs[i];i++;k++;
		}
		else{
			final.pairs[k]=temp.pairs[j];j++;k++;
		};
	while(i<spair->npairs) {final.pairs[k]=spair->pairs[i];i++;k++;};
	while(j<temp.npairs) {final.pairs[k]=temp.pairs[j];j++;k++;};
	
	free((char *)temp.pairs);
	PAIRSETRELEASE(spair);
	free((char *)scratch);

	(*spair)=final;
};

#if	eager_kills
FN kill_current_pairs(p,scratch)
LMPOL *p; short *scratch;
{
	register i;

	for (i=0; i<nworkers; i++ ) {
	    if (valid_pair[i] == 1) {
		lpp_lbd_pair(current_pair[i][0],p,scratch);
		lpp_lbd_pair(current_pair[i][1],p,scratch+nvars+1);
		lpp_lbd_pair(current_pair[i][0],current_pair[i][1],
						scratch+2*(nvars+1));
		if (expostrictdiv(scratch,scratch+2*(nvars+1)) &&
		    expostrictdiv(scratch+nvars+1,scratch+2*(nvars+1))) {
			kill_pair[i] = 1;
	  	}
	    }
	}
}
#endif	eager_kills

#if	full_kills
FN kill_finished_pairs(p,scratch)
LMPOL *p; short *scratch;
{
	struct finished_pair *pair;

	pair = finished_pair_list;
	while (pair != NULL) {
	    if (!pair->killed) {
		lpp_lbd_pair(pair->p1,p,scratch);
		lpp_lbd_pair(pair->p2,p,scratch+nvars+1);
		lpp_lbd_pair(pair->p1,pair->p2,scratch+2*(nvars+1));
		if (expostrictdiv(scratch,scratch+2*(nvars+1)) &&
		    expostrictdiv(scratch+nvars+1,scratch+2*(nvars+1))) {
			pair->killed = 1;
			printf("killed finished pair (%d,%d) using %d\n",
			       pair->p1->label,pair->p2->label,p->label);
			fflush(stdout);
	  	}
	    }
	    pair = pair->next_pair;
	}
}

FN add_to_finished_pairs(p1,p2,temp)
LMPOL *p1,*p2;
PAIRSET *temp;
{
  struct finished_pair *fp;
  int i;

  fp = (struct finished_pair *) galloc(sizeof(struct finished_pair));
  fp->p1 = p1; 
  fp->p2 = p2;
  fp->killed = 0;
  fp->nchildren = temp->npairs;
  if (temp->npairs>0) {
    fp->children = (int *) galloc(sizeof(int)*temp->npairs);
  }
  for (i=0; i < temp->npairs; i++) {
    fp->children[i] = (temp->pairs[i].p1->label) * 1000 +
      temp->pairs[i].p2->label;
  }
  fp->next_pair = finished_pair_list;
  finished_pair_list = fp->next_pair;
}
#endif	full_kills

update_old_pairs(spair,p,scratch) 
	PAIRSET *spair; LMPOL *p; short *scratch;

/* Delete in the set of pairs "spair", the pairs that won't need to be reduced
due to the apparition of polynomial "p" in the basis. "scratch" is just a
pointer to a dynamically allocated array.
*/

{	
	register pos,i;
	int new_npairs;

	pos=0; new_npairs=spair->npairs;
	for (i=0;i<spair->npairs;i++){
	   	if ((spair->pairs[i].p1->old==0)&&
		    (spair->pairs[i].p2->old==0)){
			lpp_lbd_pair(spair->pairs[i].p1,p,scratch);
			lpp_lbd_pair(spair->pairs[i].p2,p,scratch+nvars+1);
			lpp_lbd_pair(spair->pairs[i].p1,spair->pairs[i].p2,
							scratch+2*(nvars+1));
			if (expostrictdiv(scratch,scratch+2*(nvars+1))&&
			    expostrictdiv(scratch+nvars+1,
							scratch+2*(nvars+1))){
				new_npairs--;
				spair->pairs[i].p1->np--;
				spair->pairs[i].p2->np--;

				if (spy>=3){
	 				printf("Criterium 1 applied to the pair (%d,%d) because of polynomial %d\n",
					spair->pairs[i].p1->label,
					spair->pairs[i].p2->label,p->label);
				};
			}
			else {
			  if (i!=pos) spair->pairs[pos]=spair->pairs[i];
			  pos++;
			};
		    }
		    else{
			if (i!=pos) spair->pairs[pos]=spair->pairs[i];
			pos++;
		    };
	};
	if ((new_npairs==0)&&(spair->npairs!=0))  free((char *)spair->pairs);
	spair->npairs=new_npairs;
};


build_new_pairs(spol,p,spair,scratch) 
	MPOLSET *spol; LMPOL *p; PAIRSET *spair; short *scratch;

/* This function returns the new set of pairs "spair" between "p" and the 
polynomials of "spol". It does not perform any filtration. 
"scratch" provides some space to put partial results.
*/

{
	register j;
	register LMPOL *pt;
	if ((spair->pairs = 
	      (POLPAIR *)galloc((unsigned)spol->npols*sizeof(POLPAIR)))==0)
	   valerr();

	pt=spol->pols;

	while (  (pt->old==1)||(pt==p)  ) pt=pt->next;
	spair->pairs[0].p1 = pt;
	spair->pairs[0].p2 = p;
	spair->npairs = 1;
	pt=pt->next;

	while (pt!=NULL){
	    if ((pt->old==0)&& (pt != p)){
			lpp_lbd_pair(pt,p,scratch);
			lpp_lbd_pair(spair->pairs[spair->npairs-1].p1, spair->pairs[spair->npairs-1].p2, scratch+nvars+1);
			j=spair->npairs-1;

			while ((j>=0)&&
				((*cmp_exp)(scratch,scratch+nvars+1)>=0)){
				spair->pairs[j+1]=spair->pairs[j];
				j--;
				if (j>=0)
					lpp_lbd_pair(spair->pairs[j].p1,
						     spair->pairs[j].p2,
						     scratch+nvars+1);
			};
			j++;
			spair->pairs[j].p1=pt;
			spair->pairs[j].p2=p;
			spair->npairs++;
	    };
	    pt = pt->next;
	}
}



filter_same_lpp (spair,scratch) PAIRSET *spair; short *scratch;

/* keep only one pair from all the pairs with same lpp, the one satisfying
Buchberger criterium 2, if there exists one.
*/

{	
	register i,j,flag;

	j=0;
	flag = expocrit2(MEXPO(spair->pairs[0].p1->pol,0),
			 MEXPO(spair->pairs[0].p2->pol,0));
	lpp_lbd_pair(spair->pairs[0].p1,spair->pairs[0].p2,scratch);
	for (i=1;i<spair->npairs;i++){
		lpp_lbd_pair(spair->pairs[i].p1,
			     spair->pairs[i].p2,scratch+nvars+1);
		if (expoequal(scratch,scratch+nvars+1)){
			if ((!flag)&&
				(expocrit2(
				     MEXPO(spair->pairs[i].p1->pol,0),
			   	     MEXPO(spair->pairs[i].p2->pol,0)))){
			    if (spy>=3){
		  			printf("Criterium 1 applied to the pair (%d,%d) because of polynomial %d\n",
						spair->pairs[j].p1->label,
						spair->pairs[j].p2->label,
						spair->pairs[i].p1->label);
			   };
			   flag=1;
			   spair->pairs[j]=spair->pairs[i];
			}
			else if (spy>=3){
		  		printf("Criterium 1 applied to the pair (%d,%d) because of polynomial %d\n",
					spair->pairs[i].p1->label,
					spair->pairs[i].p2->label,
					spair->pairs[j].p1->label);
			}
		}
		else {
		    j++;
		    spair->pairs[j]=spair->pairs[i];
		    flag = expocrit2( 	MEXPO(spair->pairs[i].p1->pol,0),
					MEXPO(spair->pairs[i].p2->pol,0));
		    expocopy(scratch+nvars+1,scratch);
		}
	};
	spair->npairs=j+1;
};



filter_bigger_lpp_or_crit2(spair,scratch) PAIRSET *spair; short *scratch;

/* This function cleans "spair" by eliminating all the pairs whose lpp is
smaller than the lpp of another pair in the set and by eliminating from the
resulting set all the pairs which satisfy criterium 2 of Buchberger. The 
order in which the pairs are inspected is essential. "scratch" provides some
space to store partial results. */

{
	register i,j;

	
	for (i=0;i<spair->npairs-1;i++)
		if (expocrit2(MEXPO(spair->pairs[i].p1->pol,0),
			     MEXPO(spair->pairs[i].p2->pol,0))){
			if (spy>=3){
				printf("Criterium 2 applied to the pair (%d,%d)\n",
						spair->pairs[i].p1->label,
						spair->pairs[i].p2->label);
			};
			spair->pairs[i].p1=NULL;
			}
		else {
			lpp_lbd_pair(spair->pairs[i].p1,
				     spair->pairs[i].p2,scratch);
			j=spair->npairs-1;
			lpp_lbd_pair(spair->pairs[spair->npairs-1].p1,
				     spair->pairs[spair->npairs-1].p2,scratch+nvars+1);

			while ((j>i)&&(!expodiv(scratch+nvars+1,scratch))){
				j--;
				if (j>i) 
					lpp_lbd_pair(spair->pairs[j].p1,
						     spair->pairs[j].p2,
					   	     scratch+nvars+1);
			};
			if (j>i){
				if (spy>=3){
		  			printf("Criterium 1 applied to the pair (%d,%d) because of polynomial %d\n",
						spair->pairs[i].p1->label,
						spair->pairs[i].p2->label,
						spair->pairs[j].p1->label);
				};
				spair->pairs[i].p1=NULL;
			};
		};
	if (expocrit2(MEXPO(spair->pairs[spair->npairs-1].p1->pol,0),
		      MEXPO(spair->pairs[spair->npairs-1].p2->pol,0))){
		if (spy>=3){
			printf("Criterium 2 applied to the pair (%d,%d)\n",
				spair->pairs[spair->npairs-1].p1->label,
				spair->pairs[spair->npairs-1].p2->label);
		};
		spair->pairs[spair->npairs-1].p1=NULL;
		};

/* Now, we clean spair and update the field "np" */

	j=0;
	for (i=0;i<spair->npairs;i++)
		if (spair->pairs[i].p1!=NULL){
			spair->pairs[j]=spair->pairs[i]; 
			spair->pairs[i].p1->np++;
			spair->pairs[i].p2->np++;

			j++;
		};
	spair->npairs=j;
};



order_cmp_pair(sp) PAIRSET *sp;

{
	PAIRSET temp;
	register i,j;

	if (sp->npairs==0) return;
	if ((temp.pairs = 
	      (POLPAIR *)galloc((unsigned)sp->npairs*sizeof(POLPAIR)))==0)
	   valerr();

	temp.pairs[0] = sp->pairs[0];
	temp.npairs = 1;

	for (i=1;i<sp->npairs;i++){
		j=temp.npairs-1;

		while ((j>=0)&&
			 cmp_pairs(&(sp->pairs[i]),&(temp.pairs[j]))>=0){
				temp.pairs[j+1]=temp.pairs[j];
				j--;
		};
		j++;
		temp.pairs[j] = sp->pairs[i];
		temp.npairs++;
	};
	free(sp->pairs);
	sp->pairs = temp.pairs;
};



int cmp_pairs(pa1,pa2) POLPAIR *pa1,*pa2;

{	
	short *exp1,*exp2;

	if ((exp1 = (short *)galloc(2*(unsigned)(nvars+1)))==0)
		valerr();
	if ((exp2 = (short *)galloc(2*(unsigned)(nvars+1)))==0)
		valerr();
	lpp_lbd_pair(pa1->p1,pa1->p2,exp1);
	lpp_lbd_pair(pa2->p1,pa2->p2,exp2);
	if (reduction_first){
		if (expodiv(MEXPO(pa1->p1->pol,0),
					MEXPO(pa1->p2->pol,0))||
			(expodiv(MEXPO(pa1->p2->pol,0),
					MEXPO(pa1->p1->pol,0))))
		   if (expodiv(MEXPO(pa2->p1->pol,0),
					MEXPO(pa2->p2->pol,0))||
			   (expodiv(MEXPO(pa2->p2->pol,0),
					MEXPO(pa2->p1->pol,0))))
			goto yuck;
		   else 
			return(-1);
		if (expodiv(MEXPO(pa2->p1->pol,0),
					MEXPO(pa2->p2->pol,0))||
			(expodiv(MEXPO(pa2->p2->pol,0),
					MEXPO(pa2->p1->pol,0))))
		   return(1);
	};
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
		case 2: return (cmp_exp(exp1,exp2)); break;
		case 3: return (cmp_exp(exp2,exp1)); break;
};
};
		   
		
	
print_pairset(ps) PAIRSET *ps;

{ register i;

  printf("Set of pairs :\n");
  for (i=ps->npairs-1;i>=0;i--)
	printf("(%d,%d)\n",ps->pairs[i].p1->label,ps->pairs[i].p2->label);
  printf("\n");
};

