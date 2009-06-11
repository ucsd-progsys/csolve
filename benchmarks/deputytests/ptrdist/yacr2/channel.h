
/*
 *
 * channel.h
 *
 */


/*
 *
 * Includes.
 *
 */

#include "types.h"


#ifndef CHANNEL_H
#define CHANNEL_H


/*
 *
 * Defines.
 *
 */


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

#ifdef CHANNEL_CODE
 #define CHANNEL_DEF
#else
 #define CHANNEL_DEF extern
#endif

#define YACR_ARRAY NONNULL COUNT(channelNets+1)

CHANNEL_DEF ulong *	COUNT(channelColumns+1)	TOP;
CHANNEL_DEF ulong *	COUNT(channelColumns+1)	BOT;
CHANNEL_DEF ulong *	COUNT(channelNets+1)	FIRST;
CHANNEL_DEF ulong *	COUNT(channelNets+1)	LAST;
CHANNEL_DEF ulong *	COUNT(channelColumns+1)	DENSITY;
CHANNEL_DEF ulong *	COUNT(channelNets+1)	CROSSING;
CHANNEL_DEF ulong	ASSUMECONST	channelNets;
CHANNEL_DEF ulong	ASSUMECONST	channelColumns;
CHANNEL_DEF ulong	ASSUMECONST	channelTracks;


CHANNEL_DEF ulong			channelTracksCopy;
CHANNEL_DEF ulong			channelDensity;
CHANNEL_DEF ulong			channelDensityColumn;
CHANNEL_DEF char * NTS		channelFile;



/*
 *
 * Prototypes.
 *
 */

#if 1

void
BuildChannel(void);

void
DimensionChannel(void);

void
DescribeChannel(void);

void
DensityChannel(void);

#else	/* CHANNEL_CODE */

extern void
BuildChannel(void);

extern void
DimensionChannel(void);

`extern void
DescribeChannel(void);

extern void
DensityChannel(void);

#endif	/* CHANNEL_CODE */

#endif	/* CHANNEL_H */
