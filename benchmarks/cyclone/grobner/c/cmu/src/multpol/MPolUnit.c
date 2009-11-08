#define FN
#include "cmump.h"
#include "multpol.h"

extern short nvars;

void mpolunit(MPOL *p, MINT *coef, MPOL *q)
{ 
  register i;
  MINT *resco,rem;
  MPOL res;


  if (p->nterms==0) {
	mpolfree(q);
	mset(0,coef);
	return;
  };
  
  MINIT(&rem);
  MFREE(coef);
  MCOPY(&(p->coefs[0]),coef);
  for (i=1;i<p->nterms;i++)
	mgcd(coef,&(p->coefs[i]),coef);

  MPOLINIT(&res);

  res.nterms = p->nterms;
  POL_ALLOC(&res,p->nterms); 
  resco = res.coefs;
  for (i=0;i<p->nterms;i++){
	MINIT(resco);
	mdiv(&(p->coefs[i]),coef,(resco++),&rem);
	expocopy(MEXPO(p,i),MEXPO(&res,i));
  }
  mpolfree(q);
  MPOLMOVEFREE(&res,q);
  MFREE(&rem);
}
