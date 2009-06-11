/*
 *
 * assign.h
 *
 */


/*
 *
 * Includes.
 *
 */

#include "types.h"
#include "vcg.h"
#include "hcg.h"

#ifndef ASSIGN_H
#define ASSIGN_H


/*
 *
 * Defines.
 *
 */

#define	LOW		1
#define	MEDIUM		100
#define	HIGH		10000
#define	INFINITY	1000000


/*
 *
 * Types.
 *
 */


/*
 *
 * Globals.
 *
 */

#ifdef ASSIGN_CODE
 #define ASSIGN_DEF
#else
 #define ASSIGN_DEF extern
#endif

ASSIGN_DEF ulong * COUNT(channelTracks+2) * COUNT(channelNets+1)	costMatrix;
ASSIGN_DEF ulong *	COUNT(channelTracks+2)		tracksNoHCV;
ASSIGN_DEF ulong *	COUNT(channelTracks+2)		tracksNotPref;
ASSIGN_DEF ulong *	COUNT(channelTracks+2)		tracksTopNotPref;
ASSIGN_DEF ulong *	COUNT(channelTracks+2)		tracksBotNotPref;
ASSIGN_DEF ulong				cardNotPref;
ASSIGN_DEF ulong				cardTopNotPref;
ASSIGN_DEF ulong				cardBotNotPref;
ASSIGN_DEF ulong * COUNT(channelTracks+2) tracksAssign;
ASSIGN_DEF ulong * COUNT(channelNets+1) netsAssign;
ASSIGN_DEF ulong * COUNT(channelNets+1) netsAssignCopy;



/*
 *
 * Prototypes.
 *
 */

#if 1

void
AllocAssign(void);

void
FreeAssign(void);

void
NetsAssign(void);

void
MaxNetsAssign(void);

void
RightNetsAssign(void);

void
LeftNetsAssign(void);

void
Assign(nodeVCGType * YACR_ARRAY ,
       ulong * YACR_ARRAY,
       ulong);

void
Select(nodeVCGType * YACR_ARRAY,
       nodeHCGType * YACR_ARRAY,
       ulong * YACR_ARRAY,
       ulong * SAFE,
       ulong * YACR_ARRAY );

void
BuildCostMatrix(nodeVCGType * YACR_ARRAY ,
		nodeHCGType * YACR_ARRAY,
		ulong * YACR_ARRAY,
		ulong * YACR_ARRAY);

void
IdealTrack(ulong,
	   ulong,
	   ulong,
	   ulong * SAFE NONNULL);

#else	/* ASSIGN_CODE */

extern void
AllocAssign(void);

extern void
FreeAssign(void);

extern void
NetsAssign(void);

extern void
MaxNetsAssign(void);

extern void
RightNetsAssign(void);

extern void
LeftNetsAssign(void);

extern void
Assign(nodeVCGType *,
       ulong *,
       ulong);

extern void
Select(nodeVCGType *,
       nodeHCGType *,
       ulong *,
       ulong *,
       ulong *);

extern void
BuildCostMatrix(nodeVCGType *,
		nodeHCGType *,
		ulong *,
		ulong *);

extern void
IdealTrack(ulong,
	   ulong,
	   ulong,
	   ulong *);

#endif	/* ASSIGN_CODE */

#endif	/* ASSIGN_H */

