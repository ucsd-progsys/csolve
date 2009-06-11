/*
 *
 * hcg.h
 *
 */


/*
 *
 * Includes.
 *
 */

#include "types.h"


#ifndef HCG_H
#define HCG_H

/*
 *
 * Defines.
 *
 */


/*
 *
 * Globals.
 *
 */

#if defined(HCG_CODE)
  #define HCG_DEF
#else
  #define HCG_DEF extern
#endif

HCG_DEF ulong *	COUNT((channelNets+1)*(channelNets+1))  ASSUMECONST storageRootHCG;

// storageHCG is somewhere inside storageRootHCG
HCG_DEF ulong *	BND(__this, storageRootHCG + (channelNets+1)*(channelNets+1)) storageHCG;
HCG_DEF ulong					storageLimitHCG;

/*
 *
 * Types.
 *
 */

typedef struct _nodeHCGType {
  //matth: we can't annotate this YACR_ARRAY, because it's not guaranteed to
  //be NONNULL.  (It's not initialized at the allocation site, and our analysis
  //doesn't handle that anyway.)
    ulong * COUNT(channelNets+1)	netsHook;
    ulong	nets;
    ulong	netsReached;
} nodeHCGType;



HCG_DEF nodeHCGType * COUNT(channelNets+1)			HCG;



/*
 *
 * Prototypes.
 *
 */

#if 1

void
AllocHCG(void);

void
FreeHCG(void);

void
BuildHCG(void);

void
DFSClearHCG(nodeHCGType * YACR_ARRAY);

void
DumpHCG(nodeHCGType * YACR_ARRAY);

void
NoHCV(nodeHCGType * YACR_ARRAY,
      ulong,
      ulong * YACR_ARRAY ,
      ulong * NONNULL COUNT(channelTracks+2) );

#else	/* HCG_CODE */

extern void
AllocHCG(void);

extern void
FreeHCG(void);

extern void
BuildHCG(void);

extern void
DFSClearHCG(nodeHCGType * );

extern void
DumpHCG(nodeHCGType * );

extern void
NoHCV(nodeHCGType *,
      ulong,
      ulong *,
      ulong *);

#endif	/* HCG_CODE */

#endif	/* HCG_H */
