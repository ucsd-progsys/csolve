
/*
 *
 * vcg.h
 *
 */


/*
 *
 * Includes.
 *
 */

#include "types.h"
#include "channel.h"

#ifndef VCG_H
#define VCG_H

/*
 *
 * Defines.
 *
 */

#define	EMPTY			0
#define	EMPTY_HALF_EMPTY	2
#define	EMPTY_FULL		3
#define	HALF_EMPTY		4
#define	HALF_EMPTY_FULL		5
#define	FULL			6

/*
 *
 * Globals.
 *
 */

#if defined(VCG_CODE)
  #define VCG_DEF
#else
  #define VCG_DEF extern
#endif


/*
 *
 * Types.
 *
 */

typedef struct _constraintVCGType {
    ulong	top;
    ulong	bot;
    ulong	col;
    ulong	removed;
} constraintVCGType;

VCG_DEF constraintVCGType * COUNT((channelNets+1)*(channelNets+1))
     ASSUMECONST  storageRootVCG;
#pragma cilnoremove("storageRootVCG");
     
// storageVCG is somewhere in storageRootVCG
VCG_DEF constraintVCGType * BND(__this,
                                storageRootVCG + (channelNets+1)*(channelNets+1)) storageVCG;
VCG_DEF ulong					storageLimitVCG;

VCG_DEF constraintVCGType * COUNT(channelNets+1) * COUNT(channelNets+1) 		removeVCG;

// netsAboveHook is somewhere in storageRootVCG
typedef struct _nodeVCGType {
    constraintVCGType *	BND(__this, storageRootVCG + (channelNets+1)*(channelNets+1)) netsAboveHook;
    ulong			netsAbove;
    ulong			netsAboveLabel;
    ulong			netsAboveReached;
    constraintVCGType *	 COUNT(channelNets+1) netsBelowHook;
    ulong			netsBelow;
    ulong			netsBelowLabel;
    ulong			netsBelowReached;
} nodeVCGType;



VCG_DEF nodeVCGType * COUNT(channelNets+1)			VCG;

VCG_DEF ulong					removeTotalVCG;
VCG_DEF ulong *	COUNT(channelNets+1)			SCC;
VCG_DEF ulong					totalSCC;
VCG_DEF ulong * COUNT(channelNets+1)				perSCC;



/*
 *
 * Prototypes.
 *
 */

#if 1

void
AllocVCG(void);

void
FreeVCG(void);

void
BuildVCG(void);

void
DFSClearVCG(nodeVCGType * YACR_ARRAY);

void
DumpVCG(nodeVCGType * YACR_ARRAY);

void
DFSAboveVCG(nodeVCGType * YACR_ARRAY ,
	    ulong);

void
DFSBelowVCG(nodeVCGType * YACR_ARRAY ,
	    ulong);

void
SCCofVCG(nodeVCGType * YACR_ARRAY VCG,
	 ulong * YACR_ARRAY SCC,
	 ulong * YACR_ARRAY perSCC);

void
SCC_DFSAboveVCG(nodeVCGType * YACR_ARRAY ,
		ulong,
		ulong * SAFE);

void
SCC_DFSBelowVCG(nodeVCGType * YACR_ARRAY,
		ulong,
		ulong);

void
DumpSCC(ulong * YACR_ARRAY SCC,
	ulong * YACR_ARRAY perSCC );

void
AcyclicVCG(void);

void
RemoveConstraintVCG(nodeVCGType * YACR_ARRAY VCG,
		    ulong *  YACR_ARRAY SCC,
		    ulong *  YACR_ARRAY perSCC,
		    constraintVCGType * COUNT(channelNets+1) * YACR_ARRAY removeVCG);

ulong
ExistPathAboveVCG(nodeVCGType * YACR_ARRAY VCG,
		  ulong,
		  ulong);

void
LongestPathVCG(nodeVCGType * YACR_ARRAY VCG,
	       ulong);

ulong
DFSAboveLongestPathVCG(nodeVCGType * YACR_ARRAY VCG,
		       ulong);

ulong
DFSBelowLongestPathVCG(nodeVCGType * YACR_ARRAY VCG,
		       ulong);

ulong
VCV(nodeVCGType * YACR_ARRAY VCG,
    ulong,
    ulong,
    ulong *  YACR_ARRAY assign);

#else	/* VCG_CODE */

extern void
AllocVCG(void);

extern void
FreeVCG(void);

extern void
BuildVCG(void);

extern void
DFSClearVCG(nodeVCGType *);

extern void
DumpVCG(nodeVCGType *);

extern void
DFSAboveVCG(nodeVCGType *,
	    ulong);

extern void
DFSBelowVCG(nodeVCGType *,
	    ulong);

extern void
SCCofVCG(nodeVCGType *,
	 ulong *,
	 ulong *);

extern void
SCC_DFSAboveVCG(nodeVCGType *,
		ulong,
		ulong *);

extern void
SCC_DFSBelowVCG(nodeVCGType *,
		ulong,
		ulong);

extern void
DumpSCC(ulong *,
	ulong *);

extern void
AcyclicVCG(void);

extern void
RemoveConstraintVCG(nodeVCGType *,
		    ulong *,
		    ulong *,
		    constraintVCGType * *);

extern ulong
ExistPathAboveVCG(nodeVCGType *,
		  ulong,
		  ulong);

extern void
LongestPathVCG(nodeVCGType *,
	       ulong);

extern ulong
DFSAboveLongestPathVCG(nodeVCGType *,
		       ulong);

extern ulong
DFSBelowLongestPathVCG(nodeVCGType *,
		       ulong);

extern ulong
VCV(nodeVCGType *,
    ulong,
    ulong,
    ulong *);

#endif	/* VCG_CODE */

#endif	/* VCG_H */
