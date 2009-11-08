#include <stdio.h>
#include <stdlib.h>
#include <cm/cmmd.h>
#include <cm/timers.h>
#include <cmam/cmam.h>

#include "cmump.h"
#include "multpol.h"
#include "gbas.h"
#include "boot-2.h"

#ifdef _NEW
#	include "_newtaskq.h"
#else
#	include "distq.h"
#	include "heap.h"
#endif _NEW

#include "axioms.h"
#include "central.h"
#include "redq.h"
#include "end.h"

/*
	exports from main file
*/

extern int rule, balance, zerocnt, addcnt;
extern char debug[];
extern polsettype G, *setPtr;

#ifdef _NEW
#	undef POLL
#	define POLL ReductPoll()
#endif _NEW

/*
	new timer defs for grp
*/

#define	GRP_BASE	20

#define	GRP_RUN		(GRP_BASE+0)
#define	GRP_REDUCE	(GRP_BASE+1)
#define	GRP_SPOL	(GRP_BASE+2)
#define	GRP_VAL		(GRP_BASE+3)
#define	GRP_TERM	(GRP_BASE+4)
#define	GRP_SET		(GRP_BASE+5)


void shuffle ( polsettype *no, MPOL *new );
MPOL *idtoptr ( polsettype *set, int who, int which );

MPOL *OneSetReduce ( MPOL *p, polsettype *redSet, int *flagp );
int age_restrict ( MPOL *p, MPOL *q );
MPOL *OneSetInter ( MPOL *p, polsettype *redSet, int *flagp );
int goodpair ( MPOL *one, MPOL *two, polsettype *set );

void new_spol(MPOL *p,MPOL *q,MPOL *r);

void new_mpolsetadd(polsettype *s, MPOL *p);
int new_mpolsetremove ( polsettype *set, int who, int which );

void shm_invalidate ( int type, MPOL *p );
int shm_validp ( void );
int shm_no_adds ( void );
int star ( MPOL *a );
void shm_validate (void);
void add_del ( int who, int which );
int is_del ( int who, int which );

void ReductInit ( void );
int ReductPut ( MPOL *red, int *balancePtr );
MPOL *ReductGet ( int *balancePtr );

int CentralInit (void);
int	LocalEnqueue ( CP_type *newPtr );
int	LocalDequeue ( CP_type *oldPtr );
void CentralEnqueue ( CP_type *newPtr, int *balancePtr );
void InitialPairsDone ( void );
int PairsReady ( void );
int CentralDequeue ( CP_type *oldPtr, int *balancePtr );

