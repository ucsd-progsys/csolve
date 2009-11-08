#include "cmump.h"

/* mpow raises a to the b-th power mod c */

void mpow(MINT *a, MINT *b, MINT *c, MINT *result)
{	int i,j,n;
	MINT x,y,d;

	MINIT(&x);
	MINIT(&y);
	MSET (1,&d);

	for(j=0;j<b->len;j++)
	{	n=b->val[b->len-j-1];
		for(i=0;i<15;i++)
		{	mmult(&d,&d,&x);
			mdiv(&x,c,&y,&d);
			if((n=n<<1)&0100000)
			{	mmult(a,&d,&x);
				mdiv(&x,c,&y,&d);
			}
		}
	}
	MFREE(&x);
	MFREE(&y);
	MMOVEFREE(&d,result);
}

/* mipow raises (MINT) a to the (int) n power */

void mipow(MINT *a, int n, MINT *b)
{
	MINT temp, result;

	MSET (1, &result);
	MCOPY (a, &temp);

	while (n) {
		if (n & 1) mmult( &result, &temp, &result );
		mmult( &temp, &temp, &temp );
		n >>= 1;
	}

	MFREE(&temp);
	MMOVEFREE(&result,b);
}

/* HISTORY
 * 18-May-84  Lyle McGeoch (magoo) at Carnegie-Mellon University
 *	Created from code in existing mp package. *
 *	Debugged, cleaned up, and sped up. *
 */
