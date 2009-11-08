#include <stdio.h>
#define MAXVARS 100
#define xalloc galloc
#define xfree gfree

#ifdef BWGC
#define galloc(n) GC_malloc(n)
#define gstralloc(n) GC_malloc_atomic(n)
#define gfree(n) 0
#else
#define gstralloc malloc
#define galloc malloc
#define gfree free
#endif

#define INTR 1 /* 0: poll, 1: intr */

#ifndef	MINIT							/* 	Type declaration of MINT	*/
typedef struct {
	int len;
	short *val;
} MINT;

/*	Initialization macros--every new variable 
		be initialized with one of these macros	*/
#define MINIT(x) ((x)->len=0)
#define MSET(c,x) (((c)==0) ? (x)->len=0 				  \
	: (valloc((x)->val,1), (((c)>0) ? (((x)->len = 1), *(x)->val=(c)) \
				: (((x)->len = -1), *(x)->val= -(c)))))
#define MMOVE(x,y) (MMOVEFREE(x,y), MINIT(x))
#define MCOPY(x,y) (MINIT(y), mcopy(x,y))

/* Every MINT should be garbage collected with one of these before being 
	abandoned */
#define MFREE(x) (((x)->len!=0) ? (xfree((x)->val), 0) : 0)
#define MMOVEFREE(x,y) (*(y) = *(x))

/* Other useful statement macros */
#define mmove(x,y) (MFREE(y), MMOVE(x,y))
#define mset(c,x) (MFREE(x), MSET(c,x))
#define mnegate(x) ((x)->len = -(x)->len)

/*	Sign testing macro 	*/
#define mtest(x)	((x)->len)

/* 	Test if a MINT and a short have same value */
#define meqshort(p,q) (((p)->len!=1) ? 					     \
				(((p)->len!=(-1)) ? 0:((p)->val[0]==(-(q)))) \
				 : ((p)->val[0]==(q)))

/*	Bit test macro */
#define modd(x)		(((x)->len != 0) ? ((x)->val[0] & 0x1) : 0)
#define	mlowbits(x)	(((x)->len != 0) ? ((x)->val[0]) : 0)
			/* guaranteed 15 bits */

/*	Users should avoid these in general	*/
#define vfree(u) xfree((char *)u)
#define valloc(x,i) 				\
	((((x)=(short *)gstralloc((unsigned)(2*(i)))) == 0) ? valerr() : 0)


char *htonm();

/*
 * HISTORY
 *
 *  3 DEC 93 Soumen. #define flag for interrupt or poll. All .c files updated.
 *  7 JUL 92 Soumen. Renamed calls to malloc and free as xalloc and xfree so
 *           that mark-release memory allocation can be used. For grobner basis.
 *
 * 9-June-87 Jean-Philippe Vidal at Carnegie-Mellon University changed malloc
 *           and free in (*malloc_f) and (*free_f) to test several allocation
 *           policies.
 *
 * 14-Apr-87  Jean-Philippe Vidal at Carnegie-Mellon University
 *      Added the macro meqshort()
 *
 * 07-Dec-87  Bennet Yee (bsy) at Carnegie-Mellon University
 *	Added mlowbits().
 *
 * 22-Jan-87  Bennet Yee (byee) at Carnegie-Mellon University
 *	Added #ifndef MINIT to prevent multiple inclusion.
 *	Added htonm (host-to-network-mint, ala byteorder(3N)).
 *
 * 03-Dec-84  Lyle McGeoch (magoo) at Carnegie-Mellon University
 *	Created.  Based in part on existing mp package.
 * 09-apr-92
 */

/* prototypes */
#define	_proto_cmump_
#ifdef	_proto_cmump_

 extern int expoequal (short *exp1, short *exp2);
 extern void expocopy (short *exp1, short *exp2);
 extern int expozero (short *exp);
 extern void exposub (short *exp1, short *exp2, short *exp3);
 extern void expoadd (short *exp1, short *exp2, short *exp3);
 extern void expomax (short *exp1, short *exp2, short *exp3);
 extern int expodiv (register short *exp1, register short *exp2);
 extern int expostrictdiv (short *exp1, short *exp2);
 extern void expofactor (short *exp1, short *exp2, short *exp3);
 extern int expocrit2 (short *exp1, short *exp2);
 extern int cmp_lex_exp (short *exp1, short *exp2);
 extern int cmp_td_exp (short *exp1, short *exp2);
 extern int cmp_revlex_exp (short *exp1, short *exp2);
 extern int cmp_double_revlex_exp (short *exp1, short *exp2);
 extern int m_add (MINT *a, MINT *b, MINT *c);
 extern void madd (MINT *a, MINT *b, MINT *c);
 extern int m_sub (MINT *a, MINT *b, MINT *c);
 extern void msub (MINT *a, MINT *b, MINT *c);
 extern int m_dsb (int qq, int nn, short *aa, short *bb);
 extern void mdiv (MINT *a, MINT *b, MINT *q, MINT *r);
 extern void mpfatal (char *s);
 extern int mtod (MINT *a, MINT *b, double *d);
 extern int mlog (MINT *a);
 extern int mshiftl (MINT *a, int b);
 extern int mshiftr (MINT *a, int b);
 extern void mgcd (MINT *a, MINT *b, MINT *c);
 extern int minvert (MINT *a, MINT *b, MINT *c);
 extern void mmult (MINT *a, MINT *b, MINT *c);
 extern int m_in (MINT *a, int b, FILE *f);
 extern int m_in_b (MINT *a, int b, FILE *f, int blanks);
 static int slog (int n);
 extern int m_out (MINT *a, int b, FILE *f);
 extern int m_out_b (MINT *a, int b, FILE *f, int blanks);
 extern int sdiv (MINT *a, int n, MINT *q, short *r);
 extern int min (MINT *a);
 extern int omin (MINT *a);
 extern void mout (MINT *a);
 extern int omout (MINT *a);
 extern int hexmout (MINT *a);
 extern int fmout (MINT *a, FILE *f);
 extern int hexmin (MINT *a);
 extern int fmin (MINT *a, FILE *f);
 extern int Sm_out_b (char *s, MINT *a, int b, int blanks);
 extern int Sm_out (char *s, MINT *a, int b);
 extern void Smout (char *s, MINT *a);
 extern int mstrtoul (MINT *a, char *s, char **p, short int b);
 extern void mcopy (MINT *a, MINT *b);
 extern int mcmp (MINT *a, MINT *b);

void	(*PollPtr)();

#endif

#endif

