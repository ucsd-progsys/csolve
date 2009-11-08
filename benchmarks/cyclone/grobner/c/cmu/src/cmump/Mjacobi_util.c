/*
 * HISTORY
 * 07-Dec-87  Bennet Yee (bsy) at Carnegie-Mellon University
 *	Created.  Original code, not based on mp(3X).
 */
#include <cmump.h>

FN int mlowzeros(a)
MINT *a;
{
	register int	count = 0, alen;
	register short	*aval;

	alen = a->len;
	if (alen == 0) return 0;
	if (alen < 0) alen = -alen;
	for (aval = a->val; --alen >= 0 && !*aval; aval++)
		count += 15;
	for (alen = *aval; !(alen & 1); alen >>= 1)
		count++;
	return count;
}

/* we know low bit is one */
FN int mlowzeros2(a)
MINT *a;
{
	register int	count = 0, alen;
	register short	*aval;

	alen = a->len;
	if (alen == 0) return 1;
	if (alen < 0) alen = -alen;
	aval = a->val;
	if (*aval & ~1) {
		for (alen = *aval & ~1; !(alen & 1); alen >>= 1)
			count++;
		return count;
	} else {
		count = 15;
		aval++; --alen;
		if (!alen) return count;
		for (; --alen >= 0 && !*aval; aval++)
			count += 15;
		for (alen = *aval; !(alen & 1); alen >>= 1)
			count++;
		return count;
	}
}
