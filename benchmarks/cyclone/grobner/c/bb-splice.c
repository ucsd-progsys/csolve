/* buchberger's systematic interreduction */
/* ported to one cm-5 node Sat Nov 28 20:49:06 PST 1992 */

#include <cm/cmmd.h>
#include <cm/timers.h>
#include <cmam/cmam.h>

#include "cmump.h"
#include "multpol.h"
#include "gbas.h"

typedef struct npt_ {
	MPOL *p;
	struct npt_ *l;
} npt;

typedef struct nst_ {
	MPOL *p1, *p2;
	struct nst_ *l;
} nst;

/* ------------------------ instrumentatiion ---------------------------- */

extern int addcnt, zerocnt;
double redTime, minRedTime, maxRedTime;

/* ----------------------------------------------------------------------- */

short *LP ( MPOL *p ) {
	return p->expos;
}

short *LCM ( short *e1, short *e2 ) {
	static short res[4000];							/* note: no storage */
	expomax ( e1, e2, res );
	return res;
}

int divides ( short *e1, short *e2 ) {
	return expodiv ( e1, e2 );
}

/* -------------------------------------------------------------------------- */

MPOL *npt_choose ( npt **pL ) {
	if (*pL) {
		npt *throw;
		MPOL *this = (*pL)->p;
		throw = (*pL);
		*pL = (*pL)->l;
		gfree ( throw );
		return this;
	}
	else
		return 0;
}
void npt_free ( npt **pL ) {
	while ( *pL ) {
		npt *dis = *pL;
		*pL = dis->l;
		gfree ( dis );
	}
}
void npt_add ( npt **pL, MPOL *p ) {
	/* adding an element checks for duplicates by pointers only */
	npt *look;
	for ( look = *pL; look; look = look->l )
		if ( look->p == p )
			return;
	look = (npt*) galloc ( sizeof (npt) );
	look->p = p;
	look->l = (*pL);
	*pL = look;
}

npt *npt_union ( npt **pL1, npt **pL2 ) {
	npt *ans, *cursor;
	ans = 0;
	for ( cursor=*pL1; cursor; cursor=cursor->l)
		npt_add ( &ans, cursor->p );
	for ( cursor=*pL2; cursor; cursor=cursor->l)
		npt_add ( &ans, cursor->p );
	return ans;
}

int npt_in ( npt **pS, MPOL *p ) {
	npt *v;
	for ( v = *pS; v; v=v->l )
		if ( v->p == p )
			return 1;
	return 0;
}

npt *npt_minus ( npt **pS1, npt **pS2 ) {
	npt *v, *res=0;
	for ( v = *pS1; v; v=v->l )
		if ( !npt_in ( pS2, v->p ) )
			npt_add ( &res, v->p );
	return res;
}

int npt_size ( npt *pS ) {
	register int len=0; register npt *v;
	for ( v = pS; v; v=v->l )
		len++;
	return len;
}

/* ------------------------------------------------------------------------ */

int lesspair ( nst *pP1, nst *pP2 ) {
	short lcm1[4000], lcm2[4000];
	extern int (*cmp_exp)();

	lpp_lbd_pair ( pP1->p1, pP1->p2, lcm1 );
	lpp_lbd_pair ( pP2->p1, pP2->p2, lcm2 );
	if  ( (*cmp_exp) ( lcm1, lcm2 ) == 1 )
		return 0;
	else
		return 1;
}

void nst_add ( nst **pB, MPOL *p, MPOL *q ) {
	/* add only if no equal pointer pair */
	nst *look, *inspos;
	for ( look = *pB; look; look = look->l )
		if (
			( look->p1 == p && look->p2 == q ) ||
			( look->p1 == q && look->p2 == p )
		)
			return;
	look = (nst*) galloc ( sizeof(nst) );
	look->p1 = p; look->p2 = q;
	/* insertion into pair queue is tricky */
	if ( *pB ) {
		if ( lesspair ( look, *pB )) {
			look->l = *pB;
			*pB = look;
		}
		else {
			for ( inspos = (*pB); inspos->l && lesspair ( inspos, look ); inspos=inspos->l )
				;
			look->l = inspos->l;
			inspos->l = look;
		}
	}
	else {
		look->l = *pB;
		*pB = look;
	}
}

int nst_in ( nst **pB, MPOL *p, MPOL *q ) {
	nst *v;
	for ( v = (*pB); v; v = v->l )
		if (
			( v->p1 == p && v->p2 == q ) ||
			( v->p1 == q && v->p2 == p )
		)
			return 1;
	return 0;
}

/* ------------------------------------------------------------------------------ */

MPOL *NormalForm ( npt *D, MPOL *p ) {
	npt *v;

	for ( ;; ) {
		short scratch[8000];
		MPOL *t;
		MPOL *r;

		if ( p->nterms == 0 )
			return p;
		for ( v = D; v; v = v->l ) {
			r = v->p;
			if ( divides ( LP(r), LP(p) ) )
				break;
		}
		if (!v)
			return p;
		t = (MPOL*)galloc ( sizeof(MPOL));
		t->nterms = 0;
		new_redonefast ( p, r, t, scratch, 0 );

		mpolfree ( p );
		gfree ( p );
		p = t;
	}
}

/* ---------------------------------------------------------------------------- */

void ReduceAll ( npt **pR, npt **pP, npt **pG, nst **pB ) {
	while ( *pR ) {
		MPOL *h;
		npt *GUP, *newR, *t_npt;

		h = npt_choose ( pR );							/* h:= an elt of R; R:=R-{h}	*/
		GUP = npt_union ( pG, pP );
		h = NormalForm ( GUP, h );						/* h:= NormalForm(G U P, h)		*/
		npt_free ( &GUP );

		if ( h->nterms > 0 ) {
			npt *see, *G_0=0, *P_0=0;
			nst *cb, *newB=0;

			for ( see = *pG; see; see = see->l )		/* G_0:= setof g in G :			*/
				if ( divides ( LP(h), LP(see->p) ) )	/* 				LP(h) \ LP(g)	*/
					npt_add ( &G_0, see->p );
			for ( see = *pP; see; see = see->l )		/* P_0 := setof p in P :		*/
				if ( divides ( LP(h), LP(see->p) ) )	/*				LP(h) \ LP(p)	*/
					npt_add ( &P_0, see->p );

			t_npt = npt_minus ( pG, &G_0 );				/* G := G - G_0					*/
			npt_free ( pG );
			*pG = t_npt;

			t_npt = npt_minus ( pP, &P_0 );				/* P := P - P_0					*/
			npt_free ( pP );
			*pP = t_npt;

			while ( *pB ) {								/* B := B - setof <p,q>:		*/
				cb = (*pB);								/* p in G_0 or q in G_0			*/
				*pB = cb->l;
				if ( ! ( npt_in ( &G_0, cb->p1 ) || npt_in ( &G_0, cb->p2 ) ) )
					nst_add ( &newB, cb->p1, cb->p2 );
				gfree ( cb );
			}
			*pB = newB;

			t_npt = npt_union ( pR, &G_0 );				/* R := R U G_0 U P_0			*/
			npt_free ( pR );
			*pR = t_npt;
			t_npt = npt_union ( pR, &P_0 );
			npt_free ( pR );
			*pR = t_npt;
			npt_free ( &G_0 );
			npt_free ( &P_0 );
			npt_add ( pP, h );							/* P := P U set(h)				*/
		}
		else
			zerocnt++;
	}
}

/* ------------------------------------------------------------------------ */

void NewBasis ( npt **pP, npt **pG, nst **pB ) {
	npt *t_npt, *g, *p, *H, *K;

	t_npt = npt_union ( pG, pP );
	npt_free ( pG );
	*pG = t_npt;										/* G := G union P				*/

	for ( g = *pG; g; g = g->l )						/* B := B union <g,p>:			*/
		for ( p = *pP; p; p = p->l )					/* g in G and p in P, g not= p	*/
			if ( g->p != p->p )
				nst_add ( pB, g->p, p->p );
	H = (*pG); K = 0;

	while ( H ) {
		MPOL *h, *k;
		npt *single;

		h = npt_choose ( &H );							/* h:=elt of H, H := H - set(h)	*/

		single = (npt*)galloc ( sizeof(npt) );			/* k:=NormalForm(G-set(h),h)	*/
		single->p = h; single->l = 0;
		t_npt = npt_minus ( pG, &single );
		k = NormalForm ( t_npt, h );
		npt_free ( &t_npt );
		gfree ( single );

		npt_add ( &K, k );
	}
	*pG = K;
}

/* ------------------------------------------------------------------------ */

int Criterion1 ( MPOL *f1, MPOL *f2, npt **pG, nst **pB ) {
	npt *try;

	for ( try = (*pG); try; try = try->l )
		if (
			( try->p != f1 ) &&
			( try->p != f2 ) &&
			divides ( LP(try->p), LCM ( LP(f1), LP(f2) ) ) &&
			! nst_in ( pB, f1, try->p ) &&
			! nst_in ( pB, f2, try->p )
		)
			return 1;
	return 0;
}

int Criterion2 ( MPOL *f1, MPOL *f2 ) {
	short lcm[4000], prod[4000];
	lpp_lbd_pair ( f1, f2, lcm );
	expoadd ( f1->expos, f2->expos, prod );
	if ( expoequal ( prod, lcm ) )
		return 1;
	else
		return 0;
}

/* ------------------------------------------------------------------------ */

npt *Grobner ( npt **F ) {
	npt *R, *P, *G;
	nst *B;

	R = (*F);
	P = 0; G = 0; B = 0;

	ReduceAll ( &R, &P, &G, &B );
	NewBasis ( &P, &G, &B );
	addcnt = npt_size ( G );

	while ( B ) {
		nst *pair;
		MPOL *f1, *f2;

		/* status : debug */
		if (0) {
			npt *w; nst *s;
			printf ( "F:\n" );
			for ( w = (*F); w; w = w->l ) {
				mpolout ( w->p ); printf ( "\n" );
			}
			printf ( "\nP:\n" );
			for ( w = P; w; w=w->l ) {
				mpolout ( w->p ); printf ( "\n" );
			}
			printf ( "\nG:\n" );
			for ( w = G; w; w=w->l ) {
				mpolout ( w->p ); printf ( "\n" );
			}
			printf ( "\nB:\n" );
			for ( s = B; s; s = s->l ) {
				mpolout ( s->p1 ); printf (" @@@ ");
				mpolout ( s->p2 ); printf ("\n");
			}
			printf ("\n\n" );
		}

		pair = B;
		B = pair->l;
		f1 = pair->p1; f2 = pair->p2;
		gfree ( pair );

		if ( !Criterion1(f1,f2,&G,&B) && !Criterion2(f1,f2) ) {
			MPOL *h, *s = (MPOL*) galloc ( sizeof(MPOL) );

			spolfast ( f1, f2, s );
			h = NormalForm ( G, s );
			if ( h->nterms > 0 ) {
				npt *G_0=0, *test, *t_npt;
				nst *pair, *newB=0;

				for ( test = G; test; test=test->l ) {
					if  ( divides ( LP(h), LP(test->p) ) )
						npt_add ( &G_0, test->p );
				}
				R = G_0;
				test = (npt*)galloc ( sizeof(npt) );
				test->p = h; test->l = 0;
				P = test;

				t_npt = npt_minus ( &G, &G_0 );
				npt_free ( &G );
				G = t_npt;

				while ( B ) {				/* B := B - setof <p,q>:		*/
					nst *cb;
					cb = B;					/* p in G_0 or q in G_0			*/
					B = cb->l;
					if ( ! ( npt_in ( &G_0, cb->p1 ) || npt_in ( &G_0, cb->p2 ) ) )
						nst_add ( &newB, cb->p1, cb->p2 );
					gfree ( cb );
				}
				B = newB;

				ReduceAll ( &R, &P, &G, &B );
				NewBasis ( &P, &G, &B );
			}
			else {
				gfree ( h );
				zerocnt++;
			}
		}
	}
	addcnt = npt_size ( G ) - addcnt;
	return G;
}

/* ------------------------------------------------------------------------------ */

/*
   ##########################################################################
   #                                                                        #
   #                                                                        #
   #                                                                        #
   #                                                                        #
   #                                                                        #
   #                                                                        #
   ##########################################################################
*/

/* --------------------------------- globals ------------------------------------ */

int reduction_first=0;
int order_on_pairs=0;

#if 0
int order_exp=1;	/* deg */
int first_group=0;
int zero_poly_count = 0;
int redgcd = 0;
int order_on_pols=0;
int deletion=1;
int allow_same_lpp=0;
int spy = 0;
extern float itimerread();
polsettype s, pols;
#endif

double totRedTime;

extern polsettype s, pols;
polpairsettype pairs;

extern short nvars;
extern char *varnames[];

/* -------------------------------------- main ---------------------------------- */

void splice()
{
	MPOL *p, *v, *w;
	int vn, npolyin, cp;
	float oldt, newt;

	npt *start, *finish;

	int pidx=0;
	init_order_exp();

	{
		MPOL *y;
		pols.npols = 0;
		pairs.npairs = 0;
		y = s.polp;
		while ( y ) { /* have to unpack source poly's */
			MPOL *z;
			z = (MPOL*) galloc ( sizeof (MPOL) );
			z->nterms = z->maxterms = 0;
			mpolcopy ( y, z );
			z->compact = 0;
			z->next = 0;
			y = y->next;
			new_mpolsetadd ( &pols, z );
			if ( pols.npols > 1 )
				new_update_pairs ( &pairs, &pols, z );
		}
	}

	/* change format of pols from MPOL*'s to npt*'s */
	start = 0;
	{
		MPOL *y;
		for ( y = pols.polp; y; y = y->next )
			npt_add ( &start, y );
	}
	/* start values */
	if (0) {
		npt *p;
		for ( p = start; p; p = p->l ) {
			mpolout ( p->p );
			printf ( "\n" );
		}
	}

	/* start timer */
	CMMD_node_timer_clear ( _LOST_TIMER_ );
	CMMD_node_timer_start ( _LOST_TIMER_ );
	finish = Grobner ( &start );
	CMMD_node_timer_stop ( _LOST_TIMER_ );

	{
		char timestr[255];
		sprintf(timestr,"t%f a%d z%d\n",
			CMMD_node_timer_busy(_LOST_TIMER_), addcnt, zerocnt );
		hprintf(timestr);
	}

	hexit();

	/* if we get this far we might as well print answers */
	if (0) {
		npt *p; int v;

		printf ( "\n\n%d\n", (int)nvars );
		for ( v = 0; v < (int) nvars; v++ )
			printf ( "%s ", varnames[v] );
		for ( v=0, p = finish; p; p=p->l, v++ )
			;
		printf ("\n%d\n", v );
		for ( p = finish; p; p = p->l ) {
			mpolout ( p->p );
			printf ( " ;\n" );
		}
	}
}

