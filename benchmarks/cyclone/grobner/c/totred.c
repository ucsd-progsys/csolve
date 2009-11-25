#include "cmump.h"
#include "multpol.h"
#include "gbas.h"
#include <stdio.h>

#define MSIZE(x) ((x)->len>0 ? (x)->len : - ((x)->len))

extern short	nvars;
extern int		(*cmp_exp)();

/* -------------------------------------------------------------------------------- */

void redfasttotal(p,s,q,c)					/* used at the end to obtain		*/
	MPOL *p,*q;								/* the unique completely reduced	*/
	polsettype *s;							/* basis. The leading monomial is	*/
	MINT *c;								/* supposed irreducible.			*/
{
	MPOL *pt;
	int j;
	MPOL temp;
	short *scratch;

	if (p->nterms==0){
		mpolfree(q);
		return;
	};

	if ((scratch = (short *)galloc(4*(nvars+1)))==0) valerr();

	MPOLINIT(&temp);
	mpolcopy(p,&temp);
	MSET(1,c);
	j=1;
	while (j<temp.nterms){
		pt = s->polp;
		while ((temp.nterms>0)&&(pt!=NULL)){
			if (expodiv(MEXPO(pt->pol,0),MEXPO(&temp,j))){
				redonetotal(&temp,j,pt->pol,&temp,c,scratch);
				pt=s->polp;
			}
			else pt=pt->next;
		}
		j++;
	}
	mpolfree(q);
	MPOLMOVEFREE(&temp,q);
	free((char *)scratch);
}

/* -------------------------------------------------------------------------------- */

void redonetotal(p,i,q,r,c,scratch)		/* used at the end to obtain a total	*/
	MPOL *p,*q,*r;						/* reduced basis. The polynomial p is	*/
	MINT *c;							/* reduced at monomial i by q. For now,	*/
	short *scratch;						/* this is a sequential version.		*/
{
	MPOL temp;
	MINT coef1,coef2,rem;
	register ip,iq,ires;
	short *pow,*lpp;

	pow = scratch;
	lpp = scratch + nvars + 1;
	MPOLINIT(&temp);
	MINIT(&coef1); 
	MINIT(&coef2);
	MINIT(&rem);

	exposub(MEXPO(p,i),MEXPO(q,0),pow);
	mgcd(&(p->coefs[i]),&(q->coefs[0]),&coef1);
	mdiv(&(p->coefs[i]),&coef1,&coef2,&rem);
	mdiv(&(q->coefs[0]),&coef1,&coef1,&rem);
	mmult(c,&coef1,c);
	iq=1;
	ires=0;
	POL_ALLOC(&temp,p->nterms+q->nterms-2);

	for (ip=0;ip<i;ip++){
		expocopy(MEXPO(p,ip),MEXPO(&temp,ires));
		MINIT(&(temp.coefs[ires]));
		mmult(&(p->coefs[ip]),&coef1,&(temp.coefs[ires]));
		ires++;
	};
	ip++;
	if (iq<q->nterms)
		expoadd(pow,MEXPO(q,1),lpp);
	while ((ip<p->nterms)&&(iq<q->nterms))
		switch((*cmp_exp)(MEXPO(p,ip),lpp)){
		case 1 : 
			expocopy(MEXPO(p,ip),MEXPO(&temp,ires));
			MINIT(&(temp.coefs[ires]));
			mmult(&(p->coefs[ip]),&coef1,&(temp.coefs[ires]));
			ip++;
			ires++;
			break;
		case -1 : 
			expocopy(lpp,MEXPO(&temp,ires));
			MINIT(&(temp.coefs[ires]));
			mmult(&(q->coefs[iq]),
			    &coef2,
			    &(temp.coefs[ires]));
			mnegate(&(temp.coefs[ires]));
			iq++;
			ires++;
			if (iq<q->nterms) expoadd(pow,MEXPO(q,iq),lpp);
			break;
		case 0 : 
			MINIT(&(temp.coefs[ires]));
			mmult(&(p->coefs[ip]),&coef1,&(temp.coefs[ires]));
			mmult(&(q->coefs[iq]),&coef2,&rem);
			msub(&(temp.coefs[ires]),&rem,&(temp.coefs[ires]));
			if (mtest(&(temp.coefs[ires]))){
				expocopy(lpp,MEXPO(&temp,ires));
				ires++;
			};
			ip++;
			iq++;
			if (iq<q->nterms) expoadd(pow,MEXPO(q,iq),lpp);
		};
	while (ip<p->nterms) {
		expocopy(MEXPO(p,ip),MEXPO(&temp,ires));
		MINIT(&(temp.coefs[ires]));
		mmult(&(p->coefs[ip]),&coef1,&(temp.coefs[ires]));
		ip++;
		ires++;
	};
	while (iq<q->nterms){
		expoadd(pow,MEXPO(q,iq),MEXPO(&temp,ires));
		MINIT(&(temp.coefs[ires]));
		mmult(&(q->coefs[iq]),
		    &coef2,
		    &(temp.coefs[ires]));
		mnegate(&(temp.coefs[ires]));
		iq++;
		ires++;
	};
	temp.nterms = ires;

	if (ires==0){
		free((char *) temp.coefs);
		free((char *) temp.expos);
	};

	mpolfree(r);
	MPOLMOVEFREE(&temp,r);
	if (redgcd)
		mpolunit(r,&rem,r);
	MFREE(&rem);
	MFREE(&coef1);
	MFREE(&coef2);
}




eliminate_redundant_end(s) MPOLSET *s; 

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
				


