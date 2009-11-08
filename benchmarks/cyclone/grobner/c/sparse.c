#include <stdio.h>
#include <sys/time.h>
#include "cmump.h"
#include "multpol.h"
#include "gbas.h"

extern short nvars;

static float this, fraction=0;
static int cnt=0;

void sparselog ( MPOL *p ) {
	/* look at p->expos[], except those indices i such that i % (nvars+1) == 0 */
	int i, c=0;
	for ( i = 0; i < p->nterms * (nvars+1); i++ )
		if ( i % (nvars+1) )
			if ( p->expos[i] )
				c++;
	this = (float) c / (float) ( p->nterms * nvars );
	cnt++;
	fraction = (  (float) (cnt-1) * fraction + this  ) / (float) cnt;
}

float sparseout (void) {
	return fraction;
}

