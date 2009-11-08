#include "cmump.h"

msqrt(MINT *a, MINT *b, MINT *r)
{	MINT x,y,z;
	register alen,j;

	MINIT(&x); MINIT(&y); MINIT(&z);
	alen = a->len;

	if (alen<0) mpfatal("msqrt: neg arg");
	if (alen==0) {
		mset(0,b);
		mset(0,r);
		return(0);
	}

	if(alen & 01) x.len = (1+alen)/2;
	else x.len = 1 + alen/2;
	valloc(x.val,x.len);
	for (j=x.len; (--j)>=0;) x.val[j]=0;
	if (alen & 01) x.val[x.len-1]=0400;
	else x.val[x.len-1]=1;

	for (;;) {
		mdiv(a,&x,&y,&z);
		madd(&x,&y,&y);
		mshiftr(&y,1);
		if (mcmp(&x,&y) <= 0) break;
		mmove(&y,&x);
	}
	mcopy(&x,&y);
	mmult(&x,&x,&x);
	msub(a,&x,r);
	MFREE(&x);
	MMOVEFREE(&y,b);
	MFREE(&z);
	return(r->len);
}

/* HISTORY
 * 18-May-84  Lyle McGeoch (magoo) at Carnegie-Mellon University
 *	Created from code in existing mp package. *
 *	Debugged, cleaned up, and sped up. *
 */
