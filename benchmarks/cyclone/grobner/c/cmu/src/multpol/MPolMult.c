#include "cmump.h"
#include "multpol.h"

extern short nvars;

#if 0
void mpolmonmult (p,c,e,q)
MPOL *p,*q; 
MINT *c; 
short *e; 
{
	MPOL s;
	register short i,*bound;
	register short *pexp,*qexp;

	if ( !(mtest(c)) || MPOLZERO(p) )
		s.nterms = 0;
	else { 
		POL_ALLOC(&s,(s.nterms=p->nterms));
		for (i=0;i<p->nterms;i++){
			MINIT(&(s.coefs[i]));
			mmult(&(p->coefs[i]),c,&(s.coefs[i]));
		}
		pexp=p->expos; 
		qexp=s.expos;
		i = 0;
		bound=p->expos+p->nterms*(nvars+1);
		for (;pexp<bound;pexp++){
			(*(qexp++)) = (*pexp)+e[i];
			((i+1-nvars) ? (i++) :(i=0));
		};
	};

	mpolfree(q);
	MPOLMOVEFREE(&s,q);
};
#endif


void mpolmonmult (MPOL *p, MINT *c, short int *e, MPOL *q)
{
	MPOL s;
	MPOLINIT ( &s );
	if ( !(mtest(c)) || MPOLZERO(p) )
		s.nterms = 0;
	else { 
		int i;
		POL_ALLOC(&s,(s.nterms=p->nterms));
		for (i=0;i<p->nterms;i++){
#if (! INTR)
			(*PollPtr)();
#endif
			MINIT(&(s.coefs[i]));
			mmult(&(p->coefs[i]),c,&(s.coefs[i]));
			expoadd(MEXPO(p,i),e,MEXPO(&s,i));
		}
	}
	mpolfree(q);
	MPOLMOVEFREE(&s,q);
}

void mpolmult(MPOL *p, MPOL *q, MPOL *r)
{ 
	MPOL s,t;
	int i;

	MPOLINIT(&s);
	MPOLINIT(&t);
	for (i=0;i<(p->nterms);i++){
#if (! INTR)
		(*PollPtr)();
#endif
		mpolmonmult(q,&(p->coefs[i]),MEXPO(p,i),&t);
		mpoladd(&s,&t,&s);
	};
	mpolfree(&t);
	mpolfree(r);
	MPOLMOVEFREE(&s,r);
};

