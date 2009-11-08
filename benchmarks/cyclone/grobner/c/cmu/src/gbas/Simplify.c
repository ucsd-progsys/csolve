#include "cmump.h"
#include "multpol.h"
#include "gbas.h"

extern int spy;

#define FN
FN eliminate_redundant_end(s) MPOLSET *s; 

/* eliminate redundant polynomials at the end of the computation. */

{
	register LMPOL *pt1,*pt2,*pt1_old;
	register i,j;

	pt1 = s->pols;
	pt1_old=pt1;
	while (pt1!=NULL) {
		pt2 = s->pols;
		while ((pt2!=NULL) &&
			((pt1==pt2)||
			   (!expodiv(MEXPO(pt2->pol,0),
				     MEXPO(pt1->pol,0)))))
			pt2=pt2->next;
  		if (pt2!=NULL){
			if (spy>=2)
				printf("Deletion of polynomial %d\n",
							pt1->label);
			s->npols--;
			if (pt1_old!=pt1)
				pt1_old->next=pt1->next;
			else (s->pols) = pt1_old = pt1->next;
			mpolfree(pt1->pol);
			free((char *)pt1);
			if (pt1_old ==(s->pols)) 
				pt1=s->pols;
			else 
				pt1=pt1_old->next;
		}
		else {
			pt1_old=pt1;
			pt1=pt1->next;
		};
	};
	if (spy>=2) printf("\n\n");
}





complete_reduce_end(s) MPOLSET *s;

/* completely reduces the basis at the end */

{
	register LMPOL *pt; 
	MINT c;

	pt = s->pols;
	while (pt!=NULL){
		redfasttotal(pt->pol,s,pt->pol,&c);
		MFREE(&c); MINIT(&c);
		pt=pt->next;
	}
};
				


