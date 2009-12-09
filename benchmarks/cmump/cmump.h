#include <liquidc.h>

// pmr: Revert new definitions of xalloc and xfree
#define xalloc malloc
#define xfree  free
// pmr: Garbage-collecting allocation?
#define galloc malloc
#define gfree  free

#define INTR 1 /* 0: poll, 1: intr */

#undef PARALLEL
#ifdef PARALLEL
extern char *(*malloc_f)();
extern (*free_f)();
#define free free_f
#define malloc malloc_f
#else
/*extern char *malloc();*/
#endif PARALLEL

#ifndef	MINIT							/* 	Type declaration of MINT	*/
typedef struct {
	int len;
	short * ARRAY val;
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
#define MFREE(x) (((x)->len!=0) ? xfree((x)->val) : 0)
#define MMOVEFREE(x,y) ((y)->len = (x)->len, (y)->val = (x)->val) /* pmr: was (*(y) = *(x)) --- make sure ok to eval args twice! */

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
	((((x)=(short *)xalloc((unsigned)(2*(i)))) == 0) ? valerr() : 0)


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

int		expoequal ( short e1[], short e2[] );
void	expocopy ( short e1[], short e2[] );
int		expozero ( short e[] );
void	exposub ( short e1[], short e2[], short e3[] );
void	expoadd ( short e1[], short e2[], short e3[] );
void	expomax ( short e1[], short e2[], short e3[] );
int		expodiv ( short e1[], short e2[] ); 
int		expostrictdiv ( short e1[], short e2[] ); 
void	expofactor ( short e1[], short e2[], short e3[] );
int		expocrit2 ( short e1[], short e2[] ); 
void	init_order_exp ( void );

void	madd ( MINT *a, MINT *b, MINT *c );
void	msub ( MINT *a, MINT *b, MINT *c );
void	mdiv ( MINT *a, MINT *b, MINT *q, MINT *r );
void	mpfatal ( char *s );
void	mgcd ( MINT *a, MINT *b, MINT *c );
void	mmod ( MINT *a, MINT *b, MINT *r );
void	mmult ( MINT *a, MINT *b, MINT *c );
int		msize ( MINT *a );
int		min ( MINT *a );
void	mout ( MINT *a );
void	Smout ( char *s, MINT *a );
void	mpow ( MINT *a, MINT *b, MINT *c, MINT *r );
void	mipow ( MINT *a, int n, MINT *b );
void	mcopy ( MINT *a, MINT *b );
int		mcmp ( MINT *a, MINT *b );

// pmr: Don't understand function pointers
//void	(*PollPtr)();

#endif	_proto_cmump_

#endif MINIT

