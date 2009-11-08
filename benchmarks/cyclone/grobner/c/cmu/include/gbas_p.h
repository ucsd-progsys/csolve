#include "multpol.h"

#define NULL 0
#define SPYWORKER 0
#define LOCKCOUNT 1

/* Type declaration of labelled MPOL */

typedef  struct a
	{int label;
	 int old,np;
		/* used when we dare to delete polynomials 
		** np is the number of pairs, 
		** old states if it belongs to the set */
	 MPOL *pol;
	 struct a *next;
	}
LMPOL;


/* Type declaration of MPOLSET */
/* Invariant : the polynomials are listed in decreasing lexicographic order */

typedef struct
{short npols;
 LMPOL *pols;
} MPOLSET;

/* initialization of a polynomial set */

#define MPOLSETINIT(s) ((s)->npols=0,(s)->pols=NULL)


/* Type declaration of PAIRSET */

typedef struct
{LMPOL *p1;
 LMPOL *p2;
} POLPAIR;

typedef struct
{int npairs;
 POLPAIR *pairs;
} PAIRSET;




#define PAIRSETINIT(sp) ((sp)->npairs=0) 
	
#define PAIRSETRELEASE(sp)  if ((sp)->npairs!=0) free((char *)(sp)->pairs);


#define lpp_lbd_pair(p1,p2,exp) expomax(MEXPO((p1)->pol,0),               \
					MEXPO((p2)->pol,0),(exp))






/* Now, everything in relation with concurrent computation */

#include <cthreads.h>

#define MAX_WORKERS 16
#define ME *(int *)(cthread_data(cthread_self()))

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
  mutex_t		lock;
};

struct finished_pair {
  LMPOL	*p1,*p2;
  int	killed;
  int	nchildren;
  int	*children;	/* array of child pairs */
  struct finished_pair *next_pair;
};
