#include "cmump.h"
#include "multpol.h"

extern int (*cmp_exp)();
extern short nvars;

int mpolequal ( MPOL *p, MPOL *q ) {
	int t, res;
	MINT diff;

	if ( p->nterms != q->nterms )
		return 0;
	for ( t = 0; t < p->nterms * ( nvars+1 ); t++ )
		if ( p->expos[t] != q->expos[t] )
			return 0;
	for ( t = 0; t < p->nterms; t++ ) {
		MINIT ( &diff );
		msub ( &(p->coefs[t]), &(q->coefs[t]), &diff );
		res = ( diff.len == 0 );
		MFREE ( &diff );
		if ( !res )
			return 0;
	}
	return 1;
}

