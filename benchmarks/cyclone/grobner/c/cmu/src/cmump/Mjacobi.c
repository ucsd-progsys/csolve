/*
 * HISTORY
 * 07-Dec-87  Bennet Yee (bsy) at Carnegie-Mellon University
 *	Created.  Original code, not based on mp(3X).
 */
#include <cmump.h>

#define	meqone(p)	((p)->len == 1 && (p)->val[0] == 1)

FN mjacobi(pi,qi)	/* precondition:  both p and q are positive */
MINT *pi,*qi;
{
	int retval;
	MINT gcdval;

	MINIT(&gcdval); mgcd(pi,qi,&gcdval);
	if (!meqone(&gcdval)) {
		/* J(ab,cb) = J(ab,c)J(ab,b) = J(ab,c)J(0,b) = J(ab,c)*0 */
		retval = 0;
	} else	retval = mlegendre(pi,qi);
	MFREE(&gcdval);
	return retval;
}
