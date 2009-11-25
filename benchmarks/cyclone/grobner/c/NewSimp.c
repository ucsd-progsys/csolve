#include "cmump.h"
#include "multpol.h"
#include "gbas.h"

extern int spy;

/*	eliminate redundant polynomials at the end of the computation. */
new_eliminate_redundant_end(polsettype *s)
{
	register MPOL *pt1,*pt2,*pt1_old;
	register i,j;

	pt1 = s->polp;
	pt1_old=pt1;
	while(pt1!=NULL){
		pt2 = s->polp;
		while((pt2!=NULL) && ((pt1==pt2)|| (!expodiv(MEXPO(pt2,0), MEXPO(pt1,0)))))
			pt2=pt2->next;
  		if(pt2!=NULL){
			s->npols--;
			if (pt1_old!=pt1)
				pt1_old->next=pt1->next;
			else
				(s->polp) = pt1_old = pt1->next;
			mpolfree(pt1);
			gfree(pt1);
			if(pt1_old==(s->polp)) 
				pt1=s->polp;
			else 
				pt1=pt1_old->next;
		}
		else {
			pt1_old=pt1;
			pt1=pt1->next;
		}
	}
	return;
}

