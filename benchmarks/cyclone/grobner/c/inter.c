#include <stdio.h>
#include "cmump.h"
#include "multpol.h"
#include "gbas.h"

extern short nvars;
extern int (*cmp_exp)();
extern int spy, redgcd;
extern int zero_poly_count;

void inter_reduce (polsettype *polsetp, polpairsettype *pairsp)
{
	polsettype newset;
	MPOL *r, *c, *last;
	int count, throwaway;

	if ( polsetp->npols < 2 )
		return;

	for ( last = polsetp->polp; last->next; last = last->next )
		;

	newset.npols = 0;
	newset.polp = 0;

	for ( count = polsetp->npols; count > 0; count-- ) {
		/* remove the head of the poly list */
		r = polsetp->polp;
#		ifdef rel_
			printf ( "%3d:%08x--->", r->ppid, r );
#		endif rel_
		polsetp->polp = r->next;
		polsetp->npols--;
		r->next = 0;
		/* make a copy */
		c = (MPOL*) galloc ( sizeof(MPOL) );
		c->nterms = 0;
		mpolcopy ( r, c );
		c = (MPOL*) new_redfast2 ( c, polsetp->polp );
		c->ppid = r->ppid;
		c->compact = 0;
		if ( c->nterms > 0 ) {
			/* include c in newset */
			new_mpolsetadd ( &newset, c );
#			ifdef rel_
				printf ( "%3d:%08x\n", c->ppid, c );
#			endif rel_
		}
		else {
#			ifdef rel_
				printf ("\n");
				fflush(stdout);
#			endif rel_
		}
		/* append r back to the tail of polsetp */
		last->next = r;
		last = r;
		r->next = 0;
	}

	{
		MPOL *v;
		printf ( "\nold: ");
		for ( v = polsetp->polp; v; v = v->next )
			printf ( "%d ", v->ppid );
		printf ( "\nnew: ");
		for ( v = newset.polp; v; v = v->next )
			printf ( "%d ",v->ppid );
		printf ( "\n" );
	}

#	ifdef rel_
	{
		/* pair pointers */
		int op;
		printf ( "oldp:\n" );
		for ( op = 0; op < pairsp->npairs; op++ )
			printf ( "%3d:%08x--%3d:%08x\n", 
				pairsp->polpair[op].p1->ppid,  pairsp->polpair[op].p1,
				pairsp->polpair[op].p2->ppid,  pairsp->polpair[op].p2
			);
	}
#	endif rel_

	/*	free the old basis. while doing that make sure you throw away
		cancelled pairs from the pairset and change pointers to point
		at new reduced versions of polynomials that still persist */

	while ( polsetp->polp ) {
		MPOL *dispose, *search;

		dispose = polsetp->polp;
		polsetp->polp = dispose->next;

		/* search newset for same ppid */
		for ( search = newset.polp; search; search = search->next ) {
			/* if found, update pointers */
			if ( search->ppid == dispose->ppid ) {
				int px;
				for ( px = 0; px < pairsp->npairs; px++ ) {
					if ( pairsp->polpair[px].p1 == dispose )
						pairsp->polpair[px].p1 = search;
					if ( pairsp->polpair[px].p2 == dispose )
						pairsp->polpair[px].p2 = search;
				}
			}
		}
		/* if not, delete obsolete pairs */
		{
			int px = 0;
			while ( px < pairsp->npairs ) {
				if ( ( pairsp->polpair[px].p1 == dispose ) ||
					( pairsp->polpair[px].p2 == dispose ) ) {
					pairsp->polpair[px] = pairsp->polpair[pairsp->npairs-1];
					pairsp->npairs--;
				}
				else
					px++;
			}
		}

		if ( dispose->compact )
			/*gfree ( (char*) packbase ( dispose ) );*/
			printf ( "fatal error\n");
		else {
			mpolfree ( dispose );
			gfree ( (char*) dispose );
		}
	}

#	ifdef rel_
	{
		/* pair pointers */
		int op;
		printf ( "newp:\n" );
		for ( op = 0; op < pairsp->npairs; op++ )
			printf ( "%3d:%08x--%3d:%08x\n", 
				pairsp->polpair[op].p1->ppid,  pairsp->polpair[op].p1,
				pairsp->polpair[op].p2->ppid,  pairsp->polpair[op].p2
			);
	}
#	endif rel_

	/* point to new basis */
	(*polsetp) = newset;

	return;

}

