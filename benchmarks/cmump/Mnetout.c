#include "cmump.h"
#include <sys/types.h>
#include <netinet/in.h>

/*
 * This assumes that shorts can be packed immediately after ints.
 *
 * Used for network traffic via Mach ports as contrasted
 * with Unix sockets, which can be handled via the normal MINT I/O
 * routines.
 */

int msize(a)
MINT *a;
{
	register int	alen = a->len;

	alen = (alen > 0) ? alen : -alen;
	alen = alen * sizeof(short) + sizeof(long);
	return alen;
}

FN char *htonm(a)
MINT	*a;
{
	register short	*sp, *dp;
	register long	i;
	register long	alen, pktlen;
	register char	*pkt;

	alen = a->len;
	alen = (alen > 0) ? alen : -alen;
	pktlen = alen * sizeof(short) + sizeof(long);
	pkt = malloc((unsigned) pktlen);
	if (!pkt) return 0;

	*(long *)pkt = htonl((unsigned long) a->len);

	sp = a->val;
	dp = (short *) (pkt + sizeof(long));

	for (i = 0; i < alen; i++)
		*dp++ = htons((unsigned short) *sp++);
	
	return pkt;
}

FN ntohm(a,pkt)
MINT *a;
char *pkt;
{
	register short	*sp, *dp;
	register long	l;

	MFREE(a);
	l = a->len = ntohl(*(unsigned long *) pkt);
	l = (l > 0) ? l : -l;
	if (l > 0) {
		valloc(a->val,l);
		dp = a->val;
		sp = (short *) (pkt + sizeof(long));

		while (l--)
			*dp++ = ntohs((unsigned short) *sp++);
	}
}

FN int htonmb(a,b,s)
MINT	*a;
char	*b;
int	s;
{
	register short	*sp, *dp;
	register long	i;
	register long	alen, pktlen;

	alen = a->len;
	alen = (alen > 0) ? alen : -alen;
	pktlen = alen * sizeof(short) + sizeof(long);

	if (s < pktlen) return 0;

	*(long *)b = htonl((unsigned long) a->len);

	sp = a->val;
	dp = (short *) (b + sizeof(long));

	for (i = 0; i < alen; i++)
		*dp++ = htons((unsigned short) *sp++);

	return pktlen;
}
