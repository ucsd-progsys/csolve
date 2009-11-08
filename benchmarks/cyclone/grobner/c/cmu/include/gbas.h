extern char *varnames[];
extern short nvars;
/* ----------------------the incoming poly queue structures------------------------------ */
typedef struct inqt{
	int arrived;			/* wait to go 1 before reading msg */
	int msgtype;			/* not used ? */
	struct inqt *nextmsg;	/* link */
	char *message;          /* pointer to data */
} inqtype;
/* ----------------------new definitions to avoid LMPOLs--------------------------------- */
typedef struct{
	int npols;
	MPOL *polp;
} polsettype;

#if oldpair_
	typedef struct{
		MPOL *p1, *p2;
	} polpairtype;
	typedef struct{
		int npairs;
		polpairtype *polpair;
	} polpairsettype;
#endif oldpair_

#define lpp_lbd_pair(p1,p2,exp) expomax(MEXPO((p1),0), MEXPO((p2),0),(exp))
#define NULL 0

/* -------------------------- new message interface sm --------------------------------- */

#if 0
#define	REDUCE		0
#define	APPROVE		1
#define	PAIRUP		2
#define	MAXMSGTYPE	3

typedef	struct rt {
	int		from;
	int		mark;
	int		ni;
	int		base;
	struct rt	*nextreg;
} regtype;
#endif

/* ------------------------ cmu code no longer used ----------------------------------- */


#ifdef cmu_


#define FN /* */
#define SPYWORKER 0
#define LOCKCOUNT 1

/* Type declaration of labelled MPOL */

typedef  struct a {
	int label;
	int old,np;	/* used when we dare to delete polynomials 
			np is the number of pairs, 
			old states if it belongs to the set */
	MPOL *pol;
	struct a *next;
} LMPOL;


/* Type declaration of MPOLSET */
/* Invariant : the polynomials are listed in decreasing lexicographic order */

typedef struct {
	short npols;
	LMPOL *pols;
} MPOLSET;

/* initialization of a polynomial set */

#define MPOLSETINIT(s) ((s)->npols=0,(s)->pols=NULL)


/* Type declaration of PAIRSET */

typedef struct {
	LMPOL *p1;
	LMPOL *p2;
} POLPAIR;

typedef struct {
	int npairs;
	POLPAIR *pairs;
} PAIRSET;

#define PAIRSETINIT(sp) ((sp)->npairs=0) 
	
#define PAIRSETRELEASE(sp)  if ((sp)->npairs!=0) gfree((char *)(sp)->pairs);

/* #define lpp_lbd_pair(p1,p2,exp) expomax(MEXPO((p1)->pol,0), MEXPO((p2)->pol,0),(exp)) */

/* Now, everything in relation with concurrent computation */

#define MAX_WORKERS 1

struct co_record {
  int signal;
  MPOL *work_pol;
  LMPOL *next_pol;
  int who;
};

struct co_queue {
  struct co_record      entries[MAX_WORKERS];
  int                   enque_index;
  int                   deque_index;
  int                   count;
};

struct finished_pair {
  LMPOL	*p1,*p2;
  int	killed;
  int	nchildren;
  int	*children;	/* array of child pairs */
  struct finished_pair *next_pair;
};

/*
	13 apr
*/

#endif cmu_
