#include "cmump.h"
#include "multpol.h"
#include "gbas.h"


extern short nvars;
extern int (*cmp_exp)();
extern int order_on_pols;


mpolsetadd(s,p) MPOLSET *s; LMPOL *p;

{
	register LMPOL *pt1,*pt2;
	register short *pexp;
	
	pexp = MEXPO(p->pol,0);
	pt1 = s->pols;
	switch (order_on_pols) {
		case 0:		/* No special order */
			p->next=NULL;
			if (s->pols==NULL) 
				s->pols=p;
			else{
				while ((pt1->next)!=NULL) pt1=pt1->next;
				pt1->next=p; 
			};
			break;
		case 1:		/* Descending order */
			pt2=pt1;
			while ((pt1!=NULL)&&
				((*cmp_exp)(pexp,MEXPO(pt1->pol,0))<0))
				pt2=pt1;
				pt1=pt1->next;
			p->next=pt1;
			if (pt1!=pt2) pt2->next=p; else (s->pols)=p;
			break;
		case 2:		/* Ascending order */
			pt2=pt1;
			while ((pt1!=NULL)&&
				((*cmp_exp)(pexp,MEXPO(pt1->pol,0))>0))
				pt2=pt1;
				pt1=pt1->next;
			p->next=pt1;
			if (pt1!=pt2) pt2->next=p; else (s->pols)=p;
			break;
		case 3:		/* by ascending number of terms. */
			pt2=pt1;
			while ((pt1!=NULL)&&
				  (p->pol->nterms>pt1->pol->nterms))
				pt2=pt1;
				pt1=pt1->next;
			p->next=pt1;
			if (pt1!=pt2) pt2->next=p; else (s->pols) =p;
			break;
	};
	s->npols++;
}	
	


mpolsetdel_label(s,lab) MPOLSET *s; int lab;

/* The polynomial is no more useful and the space may be collected */
{
	register LMPOL *pt,*pt2;	

	pt = pt2 = s->pols;
	while (pt->label!=lab) {pt2=pt; pt=pt->next;};
	(s->npols)--;
	if (pt!=pt2) pt2->next=pt->next; else (s->pols)=pt->next;
	mpolfree(pt->pol);
	free((char *) pt);
};	

