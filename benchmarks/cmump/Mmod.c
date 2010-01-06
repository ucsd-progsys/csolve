#include <stdio.h>
#include "cmump.h"

/* m_dsb() in Mdiv.c */

#define NEGR 1

/*  See Knuth, Vol. 2 for an explanation of this algorithm   */

void mmod(MINT *a, MINT *b, MINT *r)
{
	MINT		u,v;
	short		rr;
	register	v1,v2,j;
	register short	*uj;
	int		sign, alen, blen, qlen, i, shift, v12;

#ifdef DEBUG
	printf("\n%d / %d = %d , %d", (int) a, (int) b, (int) q, (int) r);
	printf("\n"); mout(a); printf(" (%d) / ", a->len);
	mout(b);printf(" (%d)",b->len);
#endif
	alen = a->len;
	sign = 0;
	if (alen < 0) { sign = NEGR ; alen = -alen; }
	blen = b->len;
	if (blen < 0) { blen = -blen;}

	if (blen == 0) { mpfatal("mdiv divide by zero");
	} else if (alen < blen) {
		if (a != r) mcopy(a,r);
	} else if (blen == 1) {
		MINIT(&u);
		j = a->len; a->len = alen;
		smod(a,b->val[0],&rr);
		a->len = j;
		mset(rr, r);
		if (sign & NEGR) mnegate(r);
	} else {
		/* Step D1: Normalize */
		/* Make u be one word longer than a, but equal */
		{
			register short *av, *uv;
			valloc(u.val, (u.len = alen+1));
			uv = u.val;
			av = a->val;
			for (j = alen, uv[j]=0; --j >= 0;) *uv++ = *av++;
		}
		v.val	= b->val;
		v.len	= blen;

		v1	= v.val[blen-1];
		j	= 14;
		while (v1 >>= 1)  --j;
		shift = j;
		mshiftl(&u, j);
		mshiftl(&v, j);
	
		/* Step D2: Initialize */
		v1	= v.val[blen-1];
		v2	= v.val[blen-2];
		v12	= (v1<<15) | v2;
		qlen	= alen-blen+1;

		for (i = qlen, uj = &u.val[alen]; --i >= 0;  uj--) {
			/* Step D3: Calculate qhat. */
			register qhat, uj01;
			j = *uj;
			uj01 = (j<<15) | *(uj-1);
			qhat = (j == v1) ? 077777 : (uj01 / v1);
			/*
			 * j = (uj0,uj1,uj2) - qhat * (v1,v2) within 32 bits.
			 */
			j = uj01 - qhat * v1;	 /* < v1 !! */
			j = ((j<<15) | *(uj-2)) - qhat * v2;
			if (j < 0) {
				qhat--;
				if ((j + v12) < 0) qhat--;
			}
			/* Steps D4 through D6 in m_dsb. */
			if (m_dsb(qhat,blen,v.val,(uj-blen))) qhat--;
		}
		/*  Step D8: Unnormalize. */
		/*
		 * Adjust length of u.  This used to be done by calling
		 * mcan.
		 */
		uj = u.val;
		for (j=v.len; --j >= 0;) if (uj[j] != 0) break;
		u.len = ++j;
		if (j == 0) vfree(u.val);

		if (b != r) mshiftr(&v, shift);  /* unshift b */
		/* Unnormalize remainder. */
		mshiftr(&u, shift);
		mmove(&u, r);

		if (sign & NEGR) mnegate(r);
	}
#ifdef DEBUG
	printf(" = "); mout(q); printf(" (%d) , ",q->len);
	mout(r); printf(" (%d)\n", r->len);
#endif
}


smod(MINT *a, int n, short int *r)
{
	register qlen,i,x,y;
	register short *aval;
	int neg;

	qlen = a->len;
	if (qlen == 0) { *r = 0; return; }
	if (qlen > 0) neg = 0;
	else { qlen = -qlen; neg = 1;}
	if (n < 0) { n = -n; neg = 1 - neg; }

	aval = a->val + qlen;
	x = 0;
	for (i=qlen; (--i)>=0;) {
		x <<= 15;
		x += *--aval;
		y = x/n;
		x -= y*n;
	}
	if (neg) { x = -x;}
	*r = x;
}

/*
 * HISTORY
 *
 * 09-Dec-87  Bennet Yee (bsy) at CMU
 *	Created (hacked from code for mdiv).
 */
