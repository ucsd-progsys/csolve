#include "cmump.h"
#include "multpol.h"

extern int (*cmp_exp)();

void mpolsub(MPOL *p, MPOL *q, MPOL *r)
{

  register ip=0,iq=0,is=0;
  MPOL s;

  POL_ALLOC(&s,p->nterms + q->nterms);
  while ((ip<p->nterms)&&(iq<q->nterms)) {
#if (! INTR)
    (*PollPtr)();
#endif
    switch ((*cmp_exp)(MEXPO(p,ip),MEXPO(q,iq))) {
    case 1 : expocopy(MEXPO(p,ip),MEXPO(&s,is));
      MCOPY(&(p->coefs[ip]),&(s.coefs[is]));
      ip++;is++;break;
    case -1 : expocopy(MEXPO(q,iq),MEXPO(&s,is));
      MCOPY(&(q->coefs[iq]),&(s.coefs[is]));
      mnegate(&(s.coefs[is]));
      iq++;is++;break;
    case 0 : MCOPY(&(q->coefs[iq]),&(s.coefs[is]));
      mnegate(&(s.coefs[is]));
      madd(&(p->coefs[ip]),&(s.coefs[is]),&(s.coefs[is]));		
      if (mtest(&(s.coefs[is]))) {
	expocopy(MEXPO(p,ip),MEXPO(&s,is));
	is++;
      };
      ip++;iq++;
    };
  };
  while (ip<p->nterms) {
#if (! INTR)
    (*PollPtr)();
#endif
    expocopy(MEXPO(p,ip),MEXPO(&s,is));
    MCOPY(&(p->coefs[ip]),&(s.coefs[is]));
    ip++; is++;
  };
  while (iq<q->nterms) {
#if (! INTR)
    (*PollPtr)();
#endif
    expocopy(MEXPO(q,iq),MEXPO(&s,is));
    MCOPY(&(q->coefs[iq]),&(s.coefs[is]));
    mnegate(&(s.coefs[is]));
    iq++; is++;
  };
  s.nterms = is;
  if (is==0){
    xfree((char *)s.coefs);
    xfree((char *)s.expos);
  };

  mpolfree(r);	
  MPOLMOVEFREE(&s,r);
};
  
