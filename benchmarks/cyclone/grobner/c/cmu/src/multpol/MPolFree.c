#include "cmump.h"
#include "multpol.h"

void mpolfree(MPOL *p)
{ 
  int i;

#ifdef BWGC
  p->nterms = 0;
  p->coefs = 0;
  p->expos = 0;
#else
  if ((p->nterms)!=0){
    for (i=0;i<p->nterms;i++){
#if (! INTR)
        (*PollPtr)();
#endif
        MFREE(p->coefs+i);
    }
    xfree((char *) p->coefs);
    xfree((char *) p->expos);
    p->nterms=0;
  };
#endif
};
