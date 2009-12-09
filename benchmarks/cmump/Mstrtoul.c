#include <stdio.h>
#include <ctype.h>
#include "cmump.h"
#include <liquidc.h>

#undef	DEBUG

mstrtoul(MINT *a, char * ARRAY s, char * ARRAY * ARRAY p, short int b)
{
	MINT	y, base;
	int	c, dectop, alphatop;
	short	qy;
	int	i;

	mset(0,a);
	MSET(b,&base);
	y.len	= 1;
	y.val	= &qy;
	dectop = (b <= 10) ? '0' + b - 1 : '9';
	if (b > 10) alphatop = 'a' + b - 10;

	i=0;
	while (isxdigit(c=s[i++])) {
		if (isupper(c)) c = c - 'A' + 'a';
		if (c >= '0' && c <= dectop) {
			qy = c - '0';
			mmult(a,&base,a);
			if (qy != 0) madd(a,&y,a);
			continue;
		} if (b > 10 && (c >= 'a' && c <= alphatop)) {
			qy = c - 'a' + 10;
			mmult(a,&base,a);
			madd(a,&y,a);
			continue;
		}
	};
	if (p!=NULL) (*p)=(char *)s+i-1;
}



/*
 * HISTORY
 * 14-April-1989  Jean-Philippe Vidal at Carnegie-Mellon University
 * 	Complement of existing cmump package.
 */
