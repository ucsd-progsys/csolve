/* Copyright 1993 David Fotland.
 * Permission granted to Spec to use and distribute as a computer benchamrk
 * permission is not granted to use or modify this code for any
 * purpose other than benchmarking or computer system performance tuning.
 *
 * This program is available (with a user interface) for:
 * IBM-PC/DOS from Ishi Press Inc San Jose CA (408)944-9900
 * Penpoint from PenGames San Jose Ca (408)985-1236
 */

#include <stdio.h>    // sm: printf

# include "g2hd.h"
# include "g2rldef.h"

/* pattern values
 * Values are ordered by probablility for faster matching
 * Values must be in range of 0..127
 * no value can be zero
 * DC don't care 
 * WB white or black 
 * EM empty 
 * WH white
 * WE white or empty
 * BL black
 * BE black or empty
 */

# define MAXPATVAL 7
# define MINSORTVAL 5

# define DC 1
# define WE 2
# define BE 3
# define EM 4
# define WB 5
# define BL 6
# define WH 7

int vclr[] = { 2,2,1,0,2,0,0,1 }; /* a color for point value */

/* every pattern defined must have at least one point which is
 * WH 
 */

/* Where pattern is found, anywhere on board or on edge
 * if on edge, edge is at bottom of pattern
 */

# define ANYWHERE 0
# define EDGE 1


/* which edge to look for shape along */

# define LEFT 1
# define RIGHT 2
# define TOP 3
# define BOTTOM 4




/* color zero is this pattern.  Color 1 is color reversed pattern */
# define BLACKTOMOVE (color == tm)
# define WHITETOMOVE (color != tm)

# define NUMPATS 58
# define NUMPOINTS 5980

int nextpoint;
unsigned int points[NUMPOINTS];
int values[NUMPOINTS];


struct shapestruct shapes[NUMPATS*8];  /* shape patterns to match */

struct pattstruct patterns[] =
{
	{ /* 0 */
		{	/* connect a hane */
			{ DC, WH, BL, },
			{ BE, EM, WH, },
			{ DC, BE, DC, },
		},
		{	/* critical points */
			{ 0,1,0 },
			{ 4,3,2 },
			{ 0,5,0 },
		},
		ANYWHERE,TRUE	/* edge near */
	},

	{ /* 1 */
		{	/* push thru wall pattern */
			{ BL, WH, BL, },
			{ BE, EM, BE, },
		},
		{
			{ 2,1,3 },
			{ 0,4,0 },
		},
		ANYWHERE,TRUE
	},
	{ /* 2 */
		{	/* hane pattern */
			{DC, WE, DC, },
			{WE, EM, WH, },
			{EM, EM, BL, },
			{DC, EM, DC, },
		},
		{	/* critical points */
			{ 0,3,0 },  /* 0,3,9 */
			{ 4,0,1 },  /* 4,6,1 */
			{ 0,5,2 },
			{ 0,0,0 },  /* 7,0,8 */
		},
		ANYWHERE,FALSE	/* anyplace on board */
	},
	{ /* 3 */
		{	/* push through double kosumi */
			{WH, BE, DC, },
			{BE, EM, EM, },
			{BE, EM, WH, },
		},
		{	/* critical points */
			{ 1,0,4 },
			{ 0,5,0 },
			{ 3,0,2 },
		},
		ANYWHERE,TRUE	/* anyplace on board */
	},
	{ /* 4 */
		{	/* diagonal move to second line */
			{WH, EM, BL, },
			{EM, EM, EM, },
			{EM, EM, EM, },
		},
		{	/* critical points */
			{ 1,0,2 },
			{ 4,3,5 },
			{ 0,0,0 },
		},
		EDGE,TRUE	/* on edge */
	},
	{ /* 5 */
		{	/* hane on edge of board */
			{DC, WH, BL, DC, },
			{WE, EM, EM, EM, },
		},
		{	/* critical points */
			{ 0,1,2,0 },
			{ 0,0,3,0 },
		},
		EDGE,FALSE	/* on edge */
	},
	{ /* 6 */
		{	/* knights move to edge */
			{DC, BE, BL, DC, },
			{DC, EM, EM, WH, },
			{EM, EM, EM, WE, },
		},
		{	/* critical points */
			{ 0,0,1,0 },
			{ 4,0,5,2 },
			{ 0,3,0,0 },
		},
		EDGE,FALSE	/* on edge */
	},
	{ /* 7 */
		{	/* monkey jump */
			{DC, DC, BE, BL, DC, },
			{DC, EM, EM, EM, WH, },
			{EM, EM, EM, EM, WE, },
		},
		{	/* critical points */
			{ 0,0,0,1,0 },  /* 0,0,8,1,0 */
			{ 4,0,6,5,2 },  /* 4,0,6,5,2 */
			{ 0,3,0,0,0 },
		},
		EDGE,FALSE	/* on edge */
	},
	{ /* 8 */
		{	/* block expansion on 2nd line */
			{DC, BE, BE, DC, DC, },
			{BE, BE, EM, EM, WE, },
			{BE, EM, EM, EM, WH, },
			{EM, EM, EM, EM, EM, },
		},
		{	/* critical points */
			{ 0,0,4,0,0 },
			{ 3,0,0,0,0 },
			{ 0,0,2,0,1 },
			{ 0,0,0,0,0 },
		},
		EDGE,FALSE	/* anyplace on board */
	},
	{ /* 9 */
		{	/* defend territory from knights move */
			{BE, BL, EM, WE, },
			{EM, EM, EM, WH, },
			{EM, EM, EM, EM, },
			{EM, EM, EM, EM, },
		},
		{	/* critical points */
			{ 0,2,0,0 },
			{ 0,3,4,1 },
			{ 0,5,0,0 },  /* 0,5,6,0 */
			{ 0,0,0,0 },
		},
		EDGE,FALSE	/* anyplace on board */
	},
	{ /* 10 */
		{	/* jump in front of wall */
			{BE, EM, EM, EM},
			{BL, EM, EM, EM},
			{BE, EM, EM, EM},
			{BE, DC, WH, WE},
		},
		{	/* critical points */
			{ 0,0,0,0 },  /* 7,0,6,0 */
			{ 2,0,4,0 },  /* 2,0,4,0 */
			{ 0,0,0,0 },
			{ 3,0,1,5 },
		},
		ANYWHERE,FALSE	/* anyplace on board */
	},
	{ /* 11 */
		{	/* block or jump at edge */
			{BE, BE, BE, },
			{BE, EM, BE, },
			{EM, EM, WH, },
		},
		{	/* critical points */
			{ 0,0,0 },  /* 0,6,0 */
			{ 3,5,2 },  
			{ 4,0,1 },  /* 4,7,1 */
		},
		EDGE,FALSE	/* edge only */
	},
	{ /* 12 */
		{	/* edge cut / watari */
			{DC, WE, DC, },
			{BL, WH, BL, },
			{EM, EM, EM, },
		},
		{	/* critical points */
			{ 0,0,0 },
			{ 1,2,3 },
			{ 0,4,0 },
		},
		EDGE,TRUE	/* edge only */
	},
	{ /* 13 */
		{	/* edge cut / watari */
			{DC, WE, DC, DC},
			{BL, WH, EM, DC},
			{EM, EM, EM, BL},
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 1,2,5,0 },
			{ 0,4,0,3 },
		},
		EDGE,FALSE	/* anyplace on board */
	},
	{ /* 14 */
		{	/* edge cut / watari */
			{DC, DC, DC, DC},
			{DC, WH, BE, DC},
			{BL, EM, EM, BL},
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 0,2,5,0 },
			{ 1,4,0,3 },
		},
		EDGE,FALSE	/* anyplace on board */
	},
	{ /* 15 */
		{	/* edge cut / watari */
			{DC, DC, DC, },
			{DC, WH, BL, },
			{BL, EM, EM, },
		},
		{	/* critical points */
			{ 0,0,0 },
			{ 0,2,3 },
			{ 1,4,0 },
		},
		EDGE,FALSE	/* edge only */
	},
	{ /* 16 */
		{	/* push and cut knights move */
			{DC, DC, BE, BE },
			{WH, EM, EM, BE },
			{EM, EM, WH, DC},
			{BE, BE, DC, DC},
		},
		{	/* critical points */
			{ 0,0,0,5 },
			{ 1,0,0,0 },
			{ 0,4,2,0 },
			{ 0,3,0,0 },
		},
		ANYWHERE,FALSE	/* anyplace on board */
	},
	{ /* 17 */
		{	/* don't push thru wall */
			{EM, BL, DC, },
			{EM, EM, WH, },
			{EM, BL, DC, },
		},
		{	/* critical points */
			{ 0,2,0 },
			{ 5,4,1 },
			{ 0,3,0 },
		},
		ANYWHERE,TRUE	/* anyplace on board */
	},
	{ /* 18 */
		{	/* block clamp on two line */
			{BE, EM, BE, BE},
			{BL, EM, EM, BE},
			{WH, BL, EM, EM},
			{EM, EM, EM, EM},
		},
		{	/* critical points */
			{ 0,5,0,0 },
			{ 3,0,0,0 },  /* 3,0,6,0 */
			{ 1,2,4,0 },
			{ 0,0,0,0 },
		},
		EDGE,FALSE
	},
	{ /* 19 */
		{	/* invade on 3 line */
			{ EM, EM, EM, WE, WE, },
			{ EM, EM, EM, EM, WE, },
			{ EM, EM, BL, EM, WH, },
			{ EM, EM, EM, WE, WE, },
			{ EM, EM, EM, EM, EM, },
		},
		{	/* critical points */
			{ 0,0,4,0,0 },
			{ 5,0,0,0,0 },  /* 7,5,0,0,0,0 */
			{ 3,0,2,0,1 },  /* 6,3,0,2,0,1 */
			{ 0,0,0,0,0 },
			{ 0,0,0,0,0 },
		},
		EDGE,FALSE
	},
	{ /* 20 */
		{	/* jump across sector line */
			{WE, EM, EM, EM, WE },
			{EM, EM, EM, EM, EM },
			{EM, EM, EM, EM, EM, },
			{BL, EM, WH, WE, BL, },
		},
		{	/* critical points */
			{ 0,0,0,0,0 },
			{ 0,0,4,0,0 },
			{ 0,0,0,0,0 },
			{ 2,0,1,0,3 },
		},
		ANYWHERE,TRUE	/* anyplace on board */
	},
	{ /* 21 */
		{	/* jump across sector line */
			{WE, EM, EM, EM, WE },
			{EM, EM, EM, EM, EM },
			{BL, EM, EM, EM, EM, },
			{DC, WE, WH, EM, BL, },
		},
		{	/* critical points */
			{ 0,0,0,0,0 },
			{ 0,0,4,0,0 },
			{ 2,0,5,6,0 },
			{ 0,0,1,0,3 },
		},
		ANYWHERE,FALSE	/* anyplace on board */
	},
	{ /* 22 */
		{	/* jump across sector line */
			{WE, EM, EM, EM, WE },
			{BE, EM, EM, EM, BE },
			{BL, EM, EM, EM, BL, },
			{DC, WE, WH, WE, DC, },
		},
		{	/* critical points */
			{ 0,0,0,0,0 },
			{ 0,0,4,0,0 },
			{ 2,0,5,0,3 },
			{ 0,0,1,0,0 },
		},
		ANYWHERE,TRUE	/* anyplace on board */
	},
	{ /* 23 */
		{	/* jump across sector line */
			{WE, EM, EM, EM, WE },
			{EM, EM, EM, EM, BL },
			{BL, EM, EM, EM, BE, },
			{DC, WE, WH, WE, DC, },
		},
		{	/* critical points */
			{ 0,0,0,0,0 },
			{ 0,0,4,0,3 },
			{ 2,0,0,0,0 },
			{ 0,0,1,0,0 },
		},
		ANYWHERE,FALSE	/* anyplace on board */
	},
	{ /* 24 */
		{	/* knights move when pushing from behind */
			{ BL, BL, BL, BE, BE },
			{ WH, WH, EM, EM, EM },
			{ EM, EM, EM, EM, EM },
			{ EM, EM, EM, EM, EM },
		},
		{	/* critical points */
			{ 0,0,0,4,0 },
			{ 0,1,3,0,0 },
			{ 0,0,0,2,0 },
			{ 0,0,0,0,0 },
		},
		ANYWHERE,FALSE
	},
	{ /* 25 */
		{	/* jump along 4 line when overcut */
			{ BE, BL, EM, DC },
			{ EM, EM, EM, EM },
			{ WH, EM, EM, EM },
			{ WE, EM, EM, EM },
			{ EM, EM, EM, EM },
			{ EM, EM, EM, EM },
		},
		{	/* critical points */
			{ 1,2,0,0 },
			{ 0,0,0,0 },
			{ 3,0,4,0 },
			{ 0,0,5,0 },
			{ 0,0,0,0 },
			{ 0,0,0,0 },
		},
		EDGE,FALSE
	},
	{ /* 26 */
		{	/* jump along 3 line when overcut */
			{ BE, BL, EM, DC },
			{ EM, EM, EM, EM },
			{ WH, EM, EM, EM },
			{ WE, EM, EM, EM },
			{ EM, EM, EM, EM },
		},
		{	/* critical points */
			{ 1,2,0,0 },
			{ 0,0,4,0 },
			{ 3,0,5,0 },
			{ 0,0,0,0 },
			{ 0,0,0,0 },
		},
		EDGE,FALSE
	},
	{ /* 27 */
		{	/* extend 3 lib group */
			{ DC, BE, EM, WE },
			{ BL, EM, EM, EM },
			{ BE, BL, WH, EM },
			{ BE, EM, EM, EM },
			{ BE, EM, EM, WE },
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 0,0,3,0 },
			{ 0,1,2,0 },
			{ 0,0,0,0 },
			{ 0,0,0,0 },
		},
		ANYWHERE,FALSE
	},
	{ /* 28 */
		{	/* extend 3 lib group */
			{ EM, EM, EM, WE },
			{ EM, EM, EM, EM },
			{ BE, BL, WH, EM },
			{ EM, EM, EM, EM },
			{ DC, EM, EM, WE },
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 0,0,3,0 },
			{ 0,1,2,0 },
			{ 0,0,0,0 },
			{ 0,0,0,0 },
		},
		ANYWHERE,TRUE
	},
	{ /* 29 */
		{	/* outside hane from 3 lib group */
			{ BE, EM, BL, BE },
			{ EM, EM, EM, EM },
			{ EM, BL, WH, EM },
			{ EM, EM, EM, WE },
			{ EM, EM, WE, WE },
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 0,0,0,0 },
			{ 0,1,2,0 },
			{ 0,3,0,0 },
			{ 0,0,0,0 },
		},
		ANYWHERE,FALSE
	},
	{ /* 30 */
		{	/* outside hane from 3 lib group */
			{ EM, BL, EM, EM },
			{ EM, EM, EM, EM },
			{ EM, BL, WH, EM },
			{ EM, EM, EM, WE },
			{ EM, EM, WE, WE },
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 0,0,0,0 },
			{ 0,1,2,0 },
			{ 0,3,0,0 },
			{ 0,0,0,0 },
		},
		ANYWHERE,FALSE
	},
	{ /* 31 */
		{	/* one point jump into center */
			{ EM, EM, EM, WE, },
			{ EM, EM, EM, EM, },
			{ EM, EM, EM, EM, },
			{ BL, EM, WH, WE, },
		},
		{
			{ 0,0,0,0 },
			{ 0,4,3,5 },
			{ 0,0,0,0 },
			{ 1,0,2,0 },
		},
		ANYWHERE,FALSE
	},
	{ /* 32 */
		{	/* one point jump into center */
			{ EM, EM, EM, WE, },
			{ EM, EM, EM, EM, },
			{ BL, EM, EM, EM, },
			{ BE, EM, WH, WE, },
		},
		{
			{ 0,0,0,0 },
			{ 0,4,3,5 },
			{ 1,0,0,0 },
			{ 0,0,2,0 },
		},
		ANYWHERE,FALSE
	},
	{ /* 33 */
		{	/* push thru wall pattern */
			{ WE, WH, BL, },
			{ BL, EM, BE, },
		},
		{
			{ 0,1,3 },
			{ 2,4,0 },
		},
		ANYWHERE,FALSE
	},
	{ /* 34 */
		{	/* stones with two empty lines between */
			{ EM, EM, EM, EM, EM, },
			{ EM, EM, EM, EM, EM, },
			{ WE, EM, EM, EM, EM, },
			{ WE, WH, EM, EM, BL, },
		},
		{
			{ 0,0,0,0,0 },  /* 0,7,0,0,0 */
			{ 0,4,5,0,0 },
			{ 0,0,0,3,0 },  /* 6,0,0,3,0 */
			{ 0,1,0,0,2 },  /* 8,1,0,0,2 */
		},
		ANYWHERE,FALSE
	},	
	{ /* 35 */
		{	/* stones with two empty lines between */
			{ WE, WH, EM, EM, BL, },
			{ WE, EM, EM, EM, EM, },
			{ EM, EM, EM, EM, EM, },
		},
		{
			{ 0,1,0,4,2 },
			{ 0,0,0,3,0 },
			{ 0,0,0,0,0 },
		},
		EDGE,FALSE
	},	
	{ /* 36 */
		{	/* diagonal move to third line */
			{WH, EM, BL, },
			{EM, EM, EM, },
			{EM, EM, EM, },
			{EM, EM, EM, },
		},
		{	/* critical points */
			{ 1,0,2 },
			{ 4,3,5 },
			{ 0,0,0 },
			{ 0,0,0 },
		},
		EDGE,TRUE	/* on edge */
	},
	{ /* 37 */
		{	/* pull back with more liberties */
			{ DC, WH, EM, BL },
			{ WE, BL, EM, EM },
			{ WE, EM, EM, DC },
		},
		{
			{ 0,0,0,1 },
			{ 0,0,2,0 },
			{ 0,0,0,0 },
		},
		ANYWHERE,FALSE
	},
	{ /* 38 */
		{ 	/* eye protecting/stealing */
			{ DC, WH, WH, WE,},
			{ WH, EM, EM, EM,},
			{ WH, EM, EM, EM,},
			{ EM, EM, EM, EM, },
		},
		{
			{ 1,3,0,0 },
			{ 4,0,0,0 },
			{ 0,0,2,0 },
			{ 0,0,0,0 },
		},
		ANYWHERE,TRUE
	},
	{ /* 39 */
		{	/* push thru large knights (cross sector) */
			{ DC, EM, EM, DC },
			{ EM, EM, EM, BL },
			{ BL, EM, WH, DC },
		},
		{
			{ 0,0,0,0, },
			{ 0,4,5,3, },
			{ 2,0,1,0, },
		},
		ANYWHERE,FALSE
	},
	{ /* 40 */
		{	/* croscut */
			{ EM, EM, EM, EM, },
			{ EM, BL, WH, EM, },
			{ EM, WH, BL, EM, },
			{ EM, EM, EM, EM, },
		},
		{
			{ 0,0,0,0, },
			{ 0,1,2,0, },
			{ 0,3,4,0, },
			{ 0,0,0,0, },
		},
		ANYWHERE,TRUE
	},
	{ /* 41 */
		{	/* jump along 4 line when overcut */
			{ BL, EM, EM, DC },
			{ EM, EM, EM, EM },
			{ WH, EM, EM, EM },
			{ WE, EM, EM, EM },
			{ EM, EM, EM, EM },
			{ EM, EM, EM, EM },
		},
		{	/* critical points */
			{ 2,1,0,0 },
			{ 0,0,0,0 },
			{ 3,0,4,0 },
			{ 0,0,5,0 },
			{ 0,0,0,0 },
			{ 0,0,0,0 },
		},
		EDGE,FALSE
	},
	{ /* 42 */
		{	/* jump along 3 line when overcut */
			{ BL, EM, EM, DC },
			{ EM, EM, EM, EM },
			{ WH, EM, EM, EM },
			{ WE, EM, EM, EM },
			{ EM, EM, EM, EM },
		},
		{	/* critical points */
			{ 2,1,0,0 },
			{ 0,0,4,0 },
			{ 3,0,5,0 },
			{ 0,0,0,0 },
			{ 0,0,0,0 },
		},
		EDGE,FALSE
	},
	{ /* 43 */
		{	/* defend against clamp on two line or clamp */
			{ BE, DC, DC, DC },
			{ BE, BL, WH, DC },
			{ BL, WH, EM, EM },
			{ BL, WH, EM, EM },
			{ EM, EM, EM, EM },
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 0,0,0,0 },
			{ 0,0,0,0 },
			{ 1,2,3,4 },
			{ 0,0,0,0 },
		},
		EDGE,FALSE
	},
	{ /* 44 */
		{	/* jump along 4 line when overcut */
			{ BE, BE, BL, DC },
			{ EM, EM, EM, EM },
			{ WH, EM, EM, EM },
			{ WE, EM, EM, EM },
			{ EM, EM, EM, EM },
			{ EM, EM, EM, EM },
		},
		{	/* critical points */
			{ 0,1,2,0 },
			{ 0,0,0,0 },
			{ 3,0,4,0 },
			{ 0,0,5,0 },
			{ 0,0,0,0 },
			{ 0,0,0,0 },
		},
		EDGE,FALSE
	},
	{ /* 45 */
		{	/* jump along 3 line when overcut */
			{ BE, BE, BL, DC },
			{ EM, EM, EM, EM },
			{ WH, EM, EM, EM },
			{ WE, EM, EM, EM },
			{ EM, EM, EM, EM },
		},
		{	/* critical points */
			{ 0,1,2,0 },
			{ 0,0,0,0 },
			{ 3,0,4,0 },
			{ 0,0,5,0 },
			{ 0,0,0,0 },
		},
		EDGE,FALSE
	},
	{ /* 46 */
		{	/* jump across sector line */
			{WE, EM, EM, BE, },
			{EM, EM, EM, BL, },
			{EM, EM, EM, BE, },
			{BL, EM, WH, DC, },
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 0,0,0,3 },
			{ 0,4,0,0 },
			{ 2,0,1,0 },
		},
		ANYWHERE,TRUE	/* anyplace on board */
	},
	{ /* 47 */
		{	/* monkey jump */
			{DC, DC, BL, EM, DC, },
			{DC, EM, EM, EM, WH, },
			{EM, EM, EM, EM, WE, },
		},
		{	/* critical points */
			{ 0,0,1,0,0 },
			{ 4,0,5,0,2 },
			{ 0,3,0,0,0 },
		},
		EDGE,FALSE	/* on edge */
	},
	{ /* 48 */
		{	/* knights move to edge */
			{DC, BL, EM, DC, },
			{DC, EM, EM, WH, },
			{EM, EM, EM, WE, },
		},
		{	/* critical points */
			{ 0,1,0,0 },
			{ 4,0,5,2 },
			{ 0,3,0,0 },
		},
		EDGE,FALSE	/* on edge */
	},
	{ /* 49 */
		{	/* connect groups along the edge */
			{WE, EM, EM, EM, WE },
			{WH, EM, BL, EM, WH },
			{WE, EM, EM, EM, WE, },
			{EM, EM, EM, EM, EM, },
		},
		{	/* critical points */
			{ 0,0,0,0,0 },
			{ 1,0,3,0,2 },
			{ 0,0,4,0,0 },
			{ 0,0,0,0,0 },
		},
		EDGE,TRUE	/* anyplace on board */
	},
	{ /* 50 */
		{	/* break thru hole in wall */
			{EM, EM, EM, EM, },
			{WH, EM, EM, EM, },
			{EM, EM, EM, WH, },
			{EM, BL, BE, DC, },
		},
		{	/* critical points */
			{ 0,0,0,0, },
			{ 2,4,5,0, },
			{ 0,0,0,3, },
			{ 0,1,0,0, },
		},
		ANYWHERE,FALSE	/* anyplace on board */
	},
	{ /* 51 */
		{	/* jump across sector line */
			{EM, EM, EM, EM, },
			{BL, EM, EM, EM, },
			{DC, EM, WH, BL, },
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 2,0,4,0 },
			{ 0,0,1,3 },
		},
		ANYWHERE,TRUE	/* anyplace on board */
	},
	{ /* 52 */
		{	/* edge cut / watari */
			{DC, DC, DC, DC},
			{DC, WH, EM, BL},
			{BL, EM, EM, EM},
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 0,2,5,3 },
			{ 1,4,0,0 },
		},
		EDGE,FALSE	/* anyplace on board */
	},
	{ /* 53 */
		{	/* extend on edge */
			{WH, BL, BL, EM},
			{EM, WH, EM, EM},
			{EM, EM, EM, EM},
		},
		{	/* critical points */
			{ 0,0,2,0 },
			{ 0,1,3,0 },
			{ 0,0,0,0 },
		},
		EDGE,FALSE	/* anyplace on board */
	},
	{ /* 54 */
		{	/* jump across sector line */
			{DC, EM, EM, EM, },
			{EM, EM, EM, BL, },
			{EM, EM, EM, BE, },
			{BL, WH, WH, DC, },
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 0,4,0,3 },
			{ 0,0,0,0 },
			{ 2,0,1,0 },
		},
		ANYWHERE,TRUE	/* anyplace on board */
	},
	{ /* 55 */
		{	/* monkey jump */
			{DC, BL, EM, WE, DC, },
			{DC, EM, EM, EM, WH, },
			{EM, EM, EM, EM, WE, },
		},
		{	/* critical points */
			{ 0,1,0,0,0 },
			{ 4,0,5,0,2 },
			{ 0,3,0,0,0 },
		},
		EDGE,FALSE	/* on edge */
	},
	{ /* 56 */
		{	/* edge cut / watari */
			{DC, BE, DC, DC},
			{BE, EM, DC, DC},
			{BL, WH, EM, BL},
			{EM, EM, EM, EM},
		},
		{	/* critical points */
			{ 0,0,0,0 },
			{ 0,0,0,0 },
			{ 1,2,5,3 },
			{ 0,4,0,0 },
		},
		EDGE,FALSE	/* anyplace on board */
	},
	{ /* 57 */
		{	/* push thru large knights (cross sector) */
			{ DC, EM, EM, DC },
			{ EM, EM, EM, BL },
			{ BL, WH, WH, DC },
		},
		{
			{ 0,4,0,0, },
			{ 0,0,0,3, },
			{ 2,0,1,0, },
		},
		ANYWHERE,FALSE
	},
};


/* call the function at square s for shape sh */

void callfunc(int s,int sh){
	int color,s1,s2,s3,s4;
	color = grcolor[board[s + points[shapes[sh].startpoint]]] != WHITE;
	s1 = s + shapes[sh].critical[0];
	s2 = s + shapes[sh].critical[1];
	s3 = s + shapes[sh].critical[2];
	s4 = s + shapes[sh].critical[3];
	switch(shapes[sh].pattno){
	      case 0:
		if(WHITETOMOVE)
			p_connect_hane(s,sh,color,s1,s2,s3,s4);
		break;
	
	      case 1:
		p_pushthru(s,sh,color,s1,s2,s3,s4);
		break;
		
	      case 2:
		if(WHITETOMOVE)
			p_hane(s,sh,color,s1,s2,s3,s4);
		else
			p_stophane(s,sh,color,s1,s2,s3,s4);
		break;

	      case 3:
		p_double_diag(s,sh,color,s1,s2,s3,s4);
		break;
	      case 4:
	      case 36:
		p_edge_kosumi(s,sh,color,s1,s2,s3,s4);
		break;
	      case 5:
		p_edge_hane(s,sh,color,s1,s2,s3,s4);
		break;
	      case 6:
	      case 48:
		p_edge_knights(s,sh,color,s1,s2,s3,s4);
		break;
	      case 7:
	      case 47:
	      case 55:
		p_monkey_jump(s,sh,color,s1,s2,s3,s4);
		break;
	      case 8:
		p_block_second(s,sh,color,s1,s2,s3,s4);
		break;
	      case 9:
		p_block_knights(s,sh,color,s1,s2,s3,s4);
		break;
	      case 10:
		p_jump_wall(s,sh,color,s1,s2,s3,s4);
		break;
	      case 11:
		p_first_line(s,sh,color,s1,s2,s3,s4);
		break;
	      case 12:
	      case 15:
		p_edge_cut(s,sh,color,s1,s2,s3,s4);
		break;
	      case 13:
	      case 14:
	      case 52:
		p_edge_cut2(s,sh,color,s1,s2,s3,s4);
		break;
	      case 16:
		p_knightsmove(s,sh,color,s1,s2,s3,s4);
		break;
	      case 17:
		p_defend_wall(s,sh,color,s1,s2,s3,s4);
		break;
	      case 18:
		p_clamp(s,sh,color,s1,s2,s3,s4);
		break;
	      case 19:
		p_invade3(s,sh,color,s1,s2,s3,s4);
		break;
	      case 20:
	      case 57:
		p_cross_sector_nodef(s,sh,color,s1,s2,s3,s4);
		break;
	      case 46:
	      case 51:
	      case 23:
	      case 54:
		p_cross_sector(s,sh,color,s1,s2,s3,s4);
		break;
	      case 21:
	      case 22:
	      case 39:
		p_cross_def_sector(s,sh,color,s1,s2,s3,s4);
		break;
	      case 24:
		p_knights_from_behind(s,sh,color,s1,s2,s3,s4);
		break;
	      case 25:
	      case 26:
	      case 41:
	      case 42:
	      case 44:
	      case 45:
		p_overcut(s,sh,color,s1,s2,s3,s4);
		break;
	      case 27:
	      case 28:
		p_extend_3_libs(s,sh,color,s1,s2,s3,s4);
		break;
	      case 29:
	      case 30:
		p_outside_hane(s,sh,color,s1,s2,s3,s4);
		break;
	      case 31:
		p_jump_in_center(s,sh,color,s1,s2,s3,s4);
		break;
	      case 32:
		p_knight_in_center(s,sh,color,s1,s2,s3,s4);
		break;
	      case 33:
		p_pushthru(s,sh,color,s1,s2,s3,s4);
		break;
	      case 34:
		p_twopoint(s,sh,color,s1,s2,s3,s4);
		break;
	      case 35:
		p_two_point_edge(s,sh,color,s1,s2,s3,s4);
		break;
	      case 37:
		p_pullback(s,sh,color,s1,s2,s3,s4);
		break;
	      case 38:
		p_eyeprotect(s,sh,color,s1,s2,s3,s4);
		break;
	      case 40:
		p_crosscut(s,sh,color,s1,s2,s3,s4);
		break;
	      case 43:
		p_clampkill(s,sh,color,s1,s2,s3,s4);
		break;
	      case 49:
		p_edge_connect(s,sh,color,s1,s2,s3,s4);
		break;
	      case 50:
	        p_break_out(s,sh,color,s1,s2,s3,s4);
		break;
	      case 53:
		p_push_once(s,sh,color,s1,s2,s3,s4);
		break;
	      case 56:
		p_edge_cut3(s,sh,color,s1,s2,s3,s4);
		break;
		}
	}


void p_push_once(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,g2,val;
	g1 = board[s1];
	g2 = board[s2];
	if(gralive[g1] == DEAD || gralive[g2] == DEAD || grthreatened[g1] ||
	   grthreatened[g2])return;
	if(BLACKTOMOVE){
		val = atk_val(grarmy[g1]);
		fire_strat_rule(s3,SURROUND_GROUP,val,NOSQUARE,NOT_URGENT);
		}
	else {
		val = def_val(grarmy[g1])+200;
		fire_strat_rule(s3,EXTEND_ONCE,val,NOSQUARE,NOT_URGENT);
		}
	}

void p_break_out(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,g1,g2,g3,val;
	s5 = s + shapes[sh].critical[4];
	g1 = board[s1];
	g2 = board[s2];
	g3 = board[s3];
	if(BLACKTOMOVE){
		val = cut_val(g2,g3);
		if(gralive[g1] > ALIVE && !grthreatened[g1])
			val += def_val(grarmy[g1]);
		if(gralive[g1] > WEAK)val += 100;
		fire_strat_rule(s4,ATTACH_TO_ESCAPE,val,NOSQUARE,
				inlist(grarmy[g1],&urgdefarmies));
		}
	else {
		val = conn_val(g2,g3);
		fire_strat_rule(s5,CONNECT_ACROSS,val,NOSQUARE,NOT_URGENT);
		}
	}

void p_edge_connect(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,g2,val;
	g1 = board[s1];
	g2 = board[s2];
	if(gralive[g1] > WEAK || gralive[g2] > WEAK || 
	   grthreatened[g1] || grthreatened[g2])return;
	if(grlibs[g1] < 4 || grlibs[g2] < 4)return;
	if(WHITETOMOVE){
		val = conn_val(g1,g2);
		if(!moveispoteye(s4,grarmy[g1]) && !moveispoteye(s4,grarmy[g2])){
			val += def_val(grarmy[g1]);
			val += def_val(grarmy[g2]);
			}
		if(val > 0)
			fire_strat_rule(s4,CONNECT_UNDER,val,NOSQUARE,NOT_URGENT);
		}
	}

void p_clampkill(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,g2;
	g1 = board[s1];
	g2 = board[s2];
	if(gralive[g1] > WEAK || gralive[g2] > WEAK || 
	   grthreatened[g1] || grthreatened[g2])return;
	if(WHITETOMOVE)
		fire_strat_rule(s4,DEFEND_CLAMP,0,NOSQUARE,NOT_URGENT);
	else
		fire_strat_rule(s3,CLAMP_2,0,NOSQUARE,NOT_URGENT);
	}

void p_crosscut(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,g2,g3,g4;
	g1 = board[s1];
	g2 = board[s2];
	g3 = board[s3];
	g4 = board[s4];
	if(WHITETOMOVE){
		if(grthreatened[g1])
			cross_capture(g1);
		if(grthreatened[g4])
			cross_capture(g4);
		if(grcnp[g2] != EOL)
			extend_to_friend(g2);
		if(grcnp[g3] != EOL)
			extend_to_friend(g3);
		if(edge[s2] < edge[s3])
			extend_along_edge(s2);
		else
			extend_along_edge(s3);
		}
	else {
		if(grthreatened[g2])
			cross_capture(g2);
		if(grthreatened[g3])
			cross_capture(g3);
		if(grcnp[g1] != EOL)
			extend_to_friend(g1);
		if(grcnp[g4] != EOL)
			extend_to_friend(g4);
		if(edge[s1] < edge[s4])
			extend_along_edge(s1);
		else
			extend_along_edge(s4);
		}
	}

void extend_to_friend(int g){
	int ptr,s;
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(ld[s] > 2)
			fire_strat_rule(s,EXTEND_CROSS_FRIEND,0,NOSQUARE,NOT_URGENT);
		}
	}

void extend_along_edge(int s){
	int ptr;
	for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr])
		if(edge[list[ptr]] == edge[s])
			fire_strat_rule(list[ptr],CROSSCUT_EXTEND,0,NOSQUARE,NOT_URGENT);
	}

void cross_capture(int g){
	int ptr,s;
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(lnbn[s] == 3)
			fire_strat_rule(s,CAPTURE_IN_CROSSCUT,0,NOSQUARE,NOT_URGENT);
		}
	}

void p_eyeprotect(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,g3,g4,eyeval2,army;
	g1 = board[s1];
	g3 = board[s3];
	g4 = board[s4];
	if(gralive[g3] > WEAK && gralive[g4] > WEAK)return;
	if(grthreatened[g3] || grthreatened[g4])return;
	army = grarmy[g3];
	if(gralive[g3] <= VERY_ALIVE && gralive[g4] <= VERY_ALIVE && 
	   (armyeyes[army] > 0 || armyeyespace[army] > 24))return;
	if(WHITETOMOVE){
		if(grcolor[g1] == color)fire_strat_rule(s2,EYE_PROTECTING,0,NOSQUARE,NOT_URGENT);
		eyeval2 = def_val(grarmy[g3]);
		if(gralive[g3] > ALIVE || gralive[g4] > ALIVE)
			fire_strat_rule(s2,MAKE_AN_EYE,eyeval2,NOSQUARE,NOT_URGENT);
		}
	else {
		if(grcolor[g1] == color)fire_strat_rule(s2,EYE_STEALING,0,NOSQUARE,NOT_URGENT);
		eyeval2 = atk_val(grarmy[g3]);
		if(gralive[g3] > ALIVE || gralive[g4] > ALIVE)
			fire_strat_rule(s2,TAKE_THE_EYE,eyeval2,NOSQUARE,NOT_URGENT);
		}
	}

void p_pullback(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1;
	g1 = board[s1];
	if(color != tm)return;
	if(gralive[g1] > WEAK)return;
	fire_strat_rule(s2,PULL_BACK_GOOD_SHAPE,0,NOSQUARE,NOT_URGENT);
	}

void p_two_point_edge(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,g2,val;
	g2 = board[s2];
	g1 = board[s1];
	if(BLACKTOMOVE)return;
	if(gralive[g2] > WEAK)return;
	if(grthreatened[g2])return;
	val = 0;
	if(gralive[g1] > ALIVE && lnbn[s4] == 3){
		val = 0;
		if(!moveispoteye(s4,grarmy[g1]) && !grthreatened[g1])
			val = def_val(grarmy[g1]);
		fire_strat_rule(s4,CONTACT_TO_LIVE,val,NOSQUARE,NOT_URGENT);
		}
	fire_strat_rule(s3,UNDERCUT_AT_TWO,val,NOSQUARE,NOT_URGENT);
	}


void p_twopoint(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,s6,s7,s8,g1,g2;
	s5 = s + shapes[sh].critical[4];
	s6 = s1+s5-s3;
	s7 = s5+s5-s3;
	s8 = s1+s4-s5;
	g1 = board[s1];
	g2 = board[s2];
	if(BLACKTOMOVE)return;
	if(gralive[g2] > WEAK)return;
	if(grthreatened[g2])return;
	if(edge[s3] == edge[s1]-1 && edge[s1] <= 5)
		fire_strat_rule(s3,UNDERCUT_AT_TWO,0,NOSQUARE,NOT_URGENT);
	else if(edge[s1] == edge[s2] && edge[s4] > edge[s1]){
		if(board[s6] != NOGROUP)
			fire_strat_rule(s7,JUMP_IN_CENTER,0,NOSQUARE,NOT_URGENT);
		else
			fire_strat_rule(s4,JUMP_IN_CENTER,0,NOSQUARE,NOT_URGENT);
		if(gralive[g1] <= ALIVE && rterv[s5][tm] < rterv[s4][tm] &&
		   rterv[s5][1-tm] > rterv[s4][1-tm])
			fire_strat_rule(s5,EXPAND_MOYO,0,NOSQUARE,NOT_URGENT);
		}
	else if(edge[s4] == edge[s1] && edge[s1] <= 5){
		if(board[s6] == NOGROUP && board[s8] == NOGROUP){
			fire_strat_rule(s4,JUMP_OVERCUT,0,NOSQUARE,NOT_URGENT);
			fire_strat_rule(s5,JUMP_OVERCUT,0,NOSQUARE,NOT_URGENT);
			if(gralive[g1] > VERY_ALIVE)
				fire_strat_rule(s4,DONT_GET_SURROUNDED,0,NOSQUARE,URGENT);
			}
		else {
			fire_strat_rule(s7,JUMP_OVERCUT,0,NOSQUARE,NOT_URGENT);
			if(gralive[g1] > VERY_ALIVE )
				fire_strat_rule(s7,DONT_GET_SURROUNDED,0,NOSQUARE,URGENT);
			}
		}
	}


void p_jump_in_center(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,g1,g2,moyo,c;
	s5 = s + shapes[sh].critical[4];
	g1 = board[s1];
	g2 = board[s2];
	if(BLACKTOMOVE)return;
	if(edge[s3] <= 4)return;
	if(gralive[g1] > WEAK)return;
	if(grthreatened[g1])return;
	c = grcolor[g2];
	moyo = lnbf[s5][tm] == 0 && rterv[s3][c] > rterv[s4][c] && rterv[s3][1-c] < rterv[s4][1-c];
	fire_strat_rule(s3,JUMP_IN_CENTER,0,NOSQUARE,NOT_URGENT);
	if(moyo){
		fire_strat_rule(s3,EXPAND_MOYO,0,NOSQUARE,NOT_URGENT);
		fire_strat_rule(s4,EXPAND_MOYO,0,NOSQUARE,NOT_URGENT);
		}
	if(lnbf[s5][tm] != 0)
		fire_strat_rule(s4,JUMP_IN_CENTER,0,NOSQUARE,NOT_URGENT);
	else if(gralive[g2] <= ALIVE && gralive[g1] > ALIVE)
		fire_strat_rule(s4,ATTACK_IN_CENTER,0,NOSQUARE,NOT_URGENT);
	}


void p_knight_in_center(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,g1,g2,moyo,c;
	s5 = s + shapes[sh].critical[4];
	g1 = board[s1];
	g2 = board[s2];
	if(gralive[g1] > WEAK)return;
	if(grthreatened[g1])return;
	if(BLACKTOMOVE){
		c = grcolor[g1];
		moyo = lnbf[s5][tm] == 0 && rterv[s3][c] < rterv[s4][c] && rterv[s3][1-c] > rterv[s4][1-c];
		fire_strat_rule(s3,JUMP_IN_CENTER,0,NOSQUARE,NOT_URGENT);
		if(moyo)
			fire_strat_rule(s3,EXPAND_MOYO,0,NOSQUARE,NOT_URGENT);
		if(lnbf[s5][tm] != 0)
			fire_strat_rule(s4,JUMP_IN_CENTER,0,NOSQUARE,NOT_URGENT);
		else if(gralive[g2] <= ALIVE && gralive[g1] > ALIVE)
			fire_strat_rule(s3,ATTACK_IN_CENTER,0,NOSQUARE,NOT_URGENT);
		return;
		}
	if(edge[s3] <= 4)return;
	c = grcolor[g2];
	moyo = lnbf[s5][tm] == 0 && rterv[s3][c] > rterv[s4][c] && rterv[s3][1-c] < rterv[s4][1-c];
	fire_strat_rule(s3,JUMP_IN_CENTER,0,NOSQUARE,NOT_URGENT);
	if(moyo){
		fire_strat_rule(s4,EXPAND_MOYO,0,NOSQUARE,NOT_URGENT);
		fire_strat_rule(s3,EXPAND_MOYO,0,NOSQUARE,NOT_URGENT);
		}
	if(lnbf[s5][tm] != 0)
		fire_strat_rule(s4,JUMP_IN_CENTER,0,NOSQUARE,NOT_URGENT);
	else if(gralive[g2] <= ALIVE && gralive[g1] > ALIVE)
		fire_strat_rule(s4,ATTACK_IN_CENTER,0,NOSQUARE,NOT_URGENT);
	}


void p_outside_hane(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1;
	g1 = board[s1];
	if(gralive[g1] > WEAK)return;
	if(WHITETOMOVE)
		fire_strat_rule(s3,OUTSIDE_HANE,0,NOSQUARE,NOT_URGENT);
	}


void p_extend_3_libs(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,g2,urg;
	g1 = board[s1];
	g2 = board[s2];
	if(gralive[g1] > WEAK)return;
	urg = inlist(grarmy[g2],&urgdefarmies) ||gralive[g2] > ALIVE && gralive[g1] <= WEAK;
	if(WHITETOMOVE)
		fire_strat_rule(s3,ATTATCH_EXTEND,0,NOSQUARE,urg);
	}

void p_overcut(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,g2,g3;
	if(BLACKTOMOVE)return;
	s5 = s + shapes[sh].critical[4];
	if(board[s1] == NOGROUP && board[s2] == NOGROUP)return;
	g3 = board[s3];
	if(gralive[g3] > WEAK)return;
	g2 = board[s2];
	if(gralive[g2] > WEAK)return;
	if(edge[s4] > 2)
		fire_strat_rule(s4,JUMP_OVERCUT,0,NOSQUARE,NOT_URGENT);
	if(gralive[g2] > ALIVE)
		fire_strat_rule(s4,ATTACK_OVERCUT,atk_val(grarmy[g2]),NOSQUARE,NOT_URGENT);
	if(edge[s4] == edge[s3] && edge[s4] > 2)
		fire_strat_rule(s5,JUMP_OVERCUT,0,NOSQUARE,NOT_URGENT);
	else if(edge[s2] > 3 || edge[s5] > 2)
		fire_strat_rule(s5,JUMP_OVERCUT,-10,NOSQUARE,NOT_URGENT);
	if(gralive[g3] > VERY_ALIVE && edge2[s5] > 3 || edge2[s5] > edge2[s3] && edge2[s3] < 6){
		fire_strat_rule(s4,DONT_GET_SURROUNDED,0,NOSQUARE,URGENT);
		fire_strat_rule(s5,DONT_GET_SURROUNDED,0,NOSQUARE,URGENT);
		}
	}


void p_knights_from_behind(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1;
	if(BLACKTOMOVE)return;
	g1 = board[s1];
	if(grlibs[g1] >= 5)
		fire_strat_rule(s2,KNIGHTS_FROM_BEHIND,0,NOSQUARE,NOT_URGENT);
	if(board[s4] != NOGROUP)
		fire_strat_rule(s3,DONT_PUSH,0,NOSQUARE,NOT_URGENT);
	}

/* g1 is group running.  g2,g3 are surrounding groups.  s4 is
 * where to run. s5 is where to block 
 * sector patterns use white as group running!
 */

void cross_sector(int g1,int g2,int g3,int s4,int s5,int color){
	int atkval = 0,cutval,urg;
	if(grthreatened[g2] || grthreatened[g3])return;
	urg = inlist(grarmy[g1],&urgdefarmies);
	if(gralive[g2] > ALIVE)
		atkval = atk_val(grarmy[g2]);
	if(gralive[g3] > ALIVE)
		atkval += atk_val(grarmy[g3]);
	if(WHITETOMOVE){
		if(gralive[g1] > WEAK && ahead > 2)return;
		fire_strat_rule(s4,CROSS_SECTOR,0,NOSQUARE,urg);
		if(grarmy[g2] != grarmy[g3]){
			cutval = cut_val(g2,g3);
			fire_strat_rule(s4,CUT_CROSS_SECTOR,cutval,NOSQUARE,NOT_URGENT);
			}
		if(atkval != 0)
			fire_strat_rule(s4,SECTOR_RUN_ATTACK,atkval,NOSQUARE,NOT_URGENT);
		}
	else {
		atkval = atk_val(grarmy[g1]);
		if(gralive[g1] > WEAK && ahead < 3)return;
		fire_strat_rule(s5,BLOCK_CROSS_SECTOR,0,NOSQUARE,NOT_URGENT);
		if(atkval != 0)
			fire_strat_rule(s5,SECTOR_ATTACK,atkval,NOSQUARE,NOT_URGENT);
		}
	}

/* use for patterns for crossing sector lines with no defensive move */

void p_cross_sector_nodef(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g3,g2,g1;
	if(color == tm)return;
	g3 = board[s3];
	g2 = board[s2];
	g1 = board[s1];
	cross_sector(g1,g2,g3,s4,s4,color);
	}

/* use for patterns for crossing sector lines with defensive  */
/* move same as running move (at s4) */

void p_cross_sector(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g3,g2,g1;
	g3 = board[s3];
	g2 = board[s2];
	g1 = board[s1];
	cross_sector(g1,g2,g3,s4,s4,color);
	}


/* use for patterns for crossing sector lines with one defense (at s5) */

void p_cross_def_sector(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,g3,g2,g1;
	s5 = s + shapes[sh].critical[4];
	g3 = board[s3];
	g2 = board[s2];
	g1 = board[s1];
	cross_sector(g1,g2,g3,s4,s5,color);
	}

void p_invade3(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,s6,s7,g1,g2,val;
	s5 = s + shapes[sh].critical[4];
	if(edge[s5] < 3)return;
	s6 = s3 + (s2 - s1)/2;
	if(board[s6] != NOGROUP)return;
	s7 = s6 + s5 - s3;
	g1 = board[s1];
	g2 = board[s2];
	if(color == tm){
		val = def_val(grarmy[g2]);
		if(grthreatened[g2])val = 0;
		fire_strat_rule(s4,DEFEND_INVASION,val,NOSQUARE,NOT_URGENT);
		if(board[s7] == NOGROUP){
			fire_strat_rule(s5,DEFEND_INVASION,0,NOSQUARE,NOT_URGENT);
			fire_strat_rule(s6,DEFEND_INVASION,0,NOSQUARE,NOT_URGENT);
			}
		}
	else {
		val = atk_val(grarmy[g2]);
		if(gralive[g1] > ALIVE)val /= 2;
		fire_strat_rule(s3,INVADE3,val,NOSQUARE,NOT_URGENT);
		}
	}


void p_clamp(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,s6,g1,g2,g3;
	s5 = s + shapes[sh].critical[4];
	s6 = s4 + s3 - s1;
	g1 = board[s1];
	g2 = board[s2];
	g3 = board[s3];
	if(gralive[g1] > WEAK || gralive[g2] > WEAK || gralive[g3] > WEAK)return;
	if(grlibs[g3] > 3)return;
	if(grlibs[g3] == 3){
		if(lnbn[s5] < 3)return;
		if(!inlist(s5,&grlbp[g3]))return;
		}
	if(color == tm)
		fire_strat_rule(s6,PREVENT_CLAMP,0,NOSQUARE,NOT_URGENT);
	else
		fire_strat_rule(s4,CLAMP,0,NOSQUARE,NOT_URGENT);
	}

/* defend wall/connect peep */

void p_defend_wall(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,g1,g2,g3,g5,connval,cutval;
	s5 = s + shapes[sh].critical[4];
	g1 = board[s1];
	g2 = board[s2];
	g3 = board[s3];
	g5 = lgr[s5];
	if(grthreatened[g1] || gralive[g1] == DEAD)return;
	if(grthreatened[g2] || gralive[g2] == DEAD)return;
	if(grthreatened[g3] || gralive[g3] == DEAD)return;
	if(BLACKTOMOVE){
		fire_strat_rule(s4,BLOCK_PEEP,0,NOSQUARE,NOT_URGENT);
		if(grarmy[g2] != grarmy[g3]){
			connval = 0;
			if(grlibs[g2] <= 3 || grlibs[g3] <= 3)
				connval += 100;
			if(s1 == mvs[msptr-1])connval += 200;
			fire_strat_rule(s4,ANSWER_PEEP,connval,NOSQUARE,NOT_URGENT);
			}
		if(ld[s5] > 1 && grcolor[g5] == grcolor[g1]){
			cutval = cut_val(g1,g5);
			fire_strat_rule(s4,PEEP_CUTS,cutval,NOSQUARE,NOT_URGENT);
			}
		}
	else {
		if(ld[s5] == NOLD && gralive[g1] <= ALIVE)
			fire_strat_rule(s4,PUSH_THRU_WALL,0,NOSQUARE,NOT_URGENT);
		}
	}



/* cut knight's move */

void p_knightsmove(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,s6,s7,g1,g2,g3,val,can_cut;
	s5 = s + shapes[sh].critical[4];
	s6 = s5+s4-s2;
	s7 = s5+s3-s4;
	if(color != tm)return;
	g1 = board[s1];
	g2 = board[s2];
	if(gralive[g1] == DEAD || gralive[g2] == DEAD)return;
	if(grthreatened[g1] || grthreatened[g2])return;
	if(board[s3] != NOGROUP)
		g3 = board[s3];
	else if(board[s3+s4-s2] != NOGROUP)
		g3 = board[s3+s4-s2];
	else
		return;
	if(gralive[g3] > WEAK)return;
	if(grthreatened[g3])return;
	if(board[s5] != NOGROUP && gralive[board[s5]] > WEAK)return;
	if(board[s6] != NOGROUP && gralive[board[s6]] > WEAK)return;
	if(board[s7] != NOGROUP && gralive[board[s7]] > WEAK)return;

	can_cut = FALSE;
	if(board[s5] != NOGROUP || board[s6] != NOGROUP || board[s7] != NOGROUP)
		can_cut = TRUE;
	else if(edge[s2] > edge[s3] && lnbn[s7] == 4 && lnbn[s6] == 4)
		can_cut = TRUE;
	if(!can_cut){
		fire_strat_rule(s4,DONT_PUSH_KNIGHTS,0,NOSQUARE,NOT_URGENT);
		return;
		}
	val = cut_val(g1,g2);
	fire_strat_rule(s4,PUSH_CUT_KNIGHTS,val,NOSQUARE,NOT_URGENT);
	}

void p_edge_cut3(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,g1,g2,g3,val,urg;
	s5 = s + shapes[sh].critical[4];
	g1 = board[s1];
	g2 = board[s2];
	g3 = board[s3];
	if(gralive[g1] == DEAD || gralive[g3] == DEAD)return;
	if(gralive[g2] > WEAK)return;
	if(tm == color){	/* watari */
		val = 100;
		urg = inlist(grarmy[g1],&urgdefarmies) || inlist(grarmy[g3],&urgdefarmies);
		if(!grthreatened[g2]){
			val += conn_val(g1,g3);
			if(!moveispoteye(s5,grarmy[g1]) && !moveispoteye(s5,grarmy[g3])){
				if(!grthreatened[g1])
					val += def_val(grarmy[g1]);
				if(!grthreatened[g3])
					val += def_val(grarmy[g3]);
				}
			}
		if(val > 0)
			fire_strat_rule(s5,WATARI,val,NOSQUARE,urg);
		}
	else {		/* cut */
		val = cut_val(g1,g3) + 100;
		if(val > 0)
			fire_strat_rule(s4,CUT_AT_EDGE,val,NOSQUARE,NOT_URGENT);
		}
	}

void p_edge_cut(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,g2,g3,val,urg;
	g1 = board[s1];
	g2 = board[s2];
	g3 = board[s3];
	if(gralive[g1] == DEAD || gralive[g3] == DEAD)return;
	if(gralive[g2] > WEAK)return;
	if(tm == color){	/* watari */
		val = 100;
		urg = inlist(grarmy[g1],&urgdefarmies) || inlist(grarmy[g3],&urgdefarmies);
		if(!grthreatened[g2]){
			val += conn_val(g1,g3);
			if(!moveispoteye(s4,grarmy[g1]) && !moveispoteye(s4,grarmy[g3])){
				if(!grthreatened[g1])
					val += def_val(grarmy[g1]);
				if(!grthreatened[g3])
					val += def_val(grarmy[g3]);
				}
			}
		if(val > 0)
			fire_strat_rule(s4,WATARI,val,NOSQUARE,urg);
		}
	else {		/* cut */
		val = cut_val(g1,g3) + 100;
		if(val > 0)
			fire_strat_rule(s4,CUT_AT_EDGE,val,NOSQUARE,NOT_URGENT);
		}
	}


void p_edge_cut2(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,g1,g2,g3,val,urg;
	s5 = s + shapes[sh].critical[4];
	g1 = board[s1];
	g2 = board[s2];
	g3 = board[s3];
	if(gralive[g1] == DEAD || gralive[g3] == DEAD)return;
	if(gralive[g2] > WEAK)return;
	if(tm == color){	/* watari */
		val = 100;
		urg = inlist(grarmy[g1],&urgdefarmies) || inlist(grarmy[g3],&urgdefarmies);
		if(!grthreatened[g2]){
			val += conn_val(g1,g3);
			if(!moveispoteye(s4,grarmy[g1]) && !moveispoteye(s4,grarmy[g3])){
				if(!grthreatened[g1])
					val += def_val(grarmy[g1]);
				if(!grthreatened[g3])
					val += def_val(grarmy[g3]);
				}
			}
		if(val > 0)
			fire_strat_rule(s4,WATARI,val,NOSQUARE,urg);
		}
	else {		/* cut */
		val = cut_val(g1,g3) + 100;
		if(val > 0){
			fire_strat_rule(s4,CUT_AT_EDGE,val,NOSQUARE,NOT_URGENT);
			fire_strat_rule(s5,CUT_AT_EDGE,val,NOSQUARE,NOT_URGENT);
			}
		}
	}

void p_first_line(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,s6,s7,g1,g2,ptr,lflag,koflag;
	s5 = s + shapes[sh].critical[4];
	s6 = s5 + s2-s1;
	s7 = s5 + s1 - s2;
	g1 = board[s1];
	g2 = board[s2];
	if(board[s2] == NOGROUP && board[s6] == NOGROUP && board[s2+s2-s1] == NOGROUP &&
		   board[s3+s3-s4] == NOGROUP)return;
	if(gralive[g1] == DEAD || gralive[g2] == DEAD)return;
	if(g2 == NOGROUP && gralive[board[s6]] == DEAD)return;
	if(tm != color){
		if(gralive[g1] > WEAK || gralive[g2] > WEAK)return;
		fire_strat_rule(s4,JUMP_ALONG_FIRST,0,NOSQUARE,NOT_URGENT);
		if(grlibs[g1] == 2)
			fire_strat_rule(s4,REV_SENTE_JUMP,0,NOSQUARE,NOT_URGENT);
		if(edge[s4] != 0 && board[s4+s4-s7] == NOGROUP && board[s3+s3-s5] == NOGROUP)
			fire_strat_rule(s4,WIDE_OPEN_JUMP,0,NOSQUARE,NOT_URGENT);
		}
	else if(g2 != NOGROUP){
		if(grlibs[g2] == 2){
			lflag = FALSE;
			for(ptr = grlbp[g2]; ptr != EOL; ptr = links[ptr])
				if(list[ptr] == s6)
					lflag = TRUE;
			}
		koflag = grsize[g1] == 1 && grlibs[g1] == 2 && 
			(ld[list[grlbp[g1]]] == 8 || ld[list[links[grlbp[g1]]]] == 8);
		if(grlibs[g1] == 2 && !koflag)
			fire_strat_rule(s7,DEF_BLOCK_FIRST,0,NOSQUARE,NOT_URGENT);
		else if((grcolor[board[s3]] == tm ||
			grcolor[board[s6]] == tm) && grlibs[g2] > 1)
			fire_strat_rule(s7,DEF_BLOCK_FIRST,0,NOSQUARE,NOT_URGENT);
		else if(grlibs[g1] > 2 && (grlibs[g2] > 2 || lflag))
			fire_strat_rule(s3,BLOCK_FIRST_JUMP,0,NOSQUARE,NOT_URGENT);
		else 
			fire_strat_rule(s5,EXTEND_TO_BLOCK,0,NOSQUARE,NOT_URGENT);
		}
	else {
		fire_strat_rule(s5,BLOCK_FIRST_LINE,0,NOSQUARE,NOT_URGENT);
		}
	}

void p_jump_wall(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,s6,s7,s8,g1,g2,val;
	s5 = s + shapes[sh].critical[4];
	s6 = s4 + (s4-s1)/2;
	s7 = s2 + (s2-s3)/2;
	s8 = s4+s5-s1;
	g1 = board[s1];
	g2 = board[s2];
	if(gralive[g1] >= SMOTHERED || gralive[g2] >= SMOTHERED )return;
	if(tm != color && gralive[g1] <= ALIVE){
		val = 200 + atk_val(grarmy[g2]);
		fire_strat_rule(s4,JUMP_BEFORE_WALL,val,NOSQUARE,NOT_URGENT);
		if(board[s5] != NOGROUP)
			fire_strat_rule(s6,JUMP_BEFORE_WALL,val,NOSQUARE,NOT_URGENT);
		}
	if(tm == color && gralive[g2] <= WEAK && !grthreatened[g1]){
		val = 200 + def_val(grarmy[g1]) + 
			atk_val(grarmy[g2]);
		fire_strat_rule(s4,TURN_FROM_WALL,val,NOSQUARE,NOT_URGENT);
		if(board[s7] != NOGROUP)
			fire_strat_rule(s8,TURN_FROM_WALL,val,NOSQUARE,NOT_URGENT);
		}
	}



void p_block_knights(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,s6,g1,g2,atkval,defval;
	s5 = s + shapes[sh].critical[4];
	s6 = s5 + s4 - s3;
	g2 = board[s2];
	g1 = board[s1];
	if(gralive[g1] > WEAK || gralive[g2] > WEAK)return;
	if(color != tm){
		defval = 0;
		if(!grthreatened[g1] && 
		   (gralive[g1] <= ALIVE || gralive[g1] > WEAK))
			defval = def_val(grarmy[g1]);  /* others handled in g23.c */
		defval += atk_val(grarmy[g2]);
		fire_strat_rule(s5,EXPAND_ALONG_EDGE,defval,NOSQUARE,
				inlist(grarmy[g1],&urgdefarmies));
		return;
		}
	if(ltrgd[s3] == 4){
		fire_strat_rule(s3,BLOCK_FROM_KNIGHT,0,NOSQUARE,NOT_URGENT);
		fire_strat_rule(s4,BLOCK_FROM_KNIGHT,0,NOSQUARE,NOT_URGENT);
		fire_strat_rule(s5,BLOCK_FROM_KNIGHT,0,NOSQUARE,NOT_URGENT);
		fire_strat_rule(s6,BLOCK_FROM_KNIGHT,0,NOSQUARE,NOT_URGENT);
		if(gralive[g1] > ALIVE && gralive[g1] <= WEAK){
			atkval = atk_val(grarmy[g1]);
			fire_strat_rule(s4,ATTACK_WITH_KOSUMI,atkval,NOSQUARE,NOT_URGENT);
			}
		}
	if(ltrgd[s3] == 8){
		fire_strat_rule(s3,SEPARATE_GROUPS,0,NOSQUARE,NOT_URGENT);
		fire_strat_rule(s5,SEPARATE_GROUPS,0,NOSQUARE,NOT_URGENT);
		fire_strat_rule(s6,SEPARATE_GROUPS,0,NOSQUARE,NOT_URGENT);
		}
	}


void p_block_second(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,atkval,defval;
	g1 = board[s1];
	if(gralive[g1] > WEAK)return;
	if(color == tm &&
		board[s3] == NOGROUP &&
		board[s4] == NOGROUP &&
		lnbf[s3][color] == 0 &&
		lnbf[s4][color] == 0)return;
	if(color == tm){
		atkval = atk_val(grarmy[g1]);
		if(atkval == 0)
			fire_strat_rule(s2,BLOCK_2_LINE,0,NOSQUARE,NOT_URGENT);
		else
		fire_strat_rule(s2,ATK_2_LINE,0,NOSQUARE,NOT_URGENT);
		return;
		}
	defval = def_val(grarmy[g1]);
	if(grthreatened[g1])defval = 0;
	if(defval == 0)
		fire_strat_rule(s2,EXTEND_2_LINE,0,NOSQUARE,NOT_URGENT);
	else
		fire_strat_rule(s2,DEF_2_LINE,defval,NOSQUARE,NOT_URGENT);
	}


void p_edge_knights(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,g1,g2,g4,cutval,atkv;
	s5 = s + shapes[sh].critical[4];
	g1 = board[s1];
	g2 = board[s2];
	g4 = board[s4];
	if(gralive[g1] > WEAK || gralive[g2] > WEAK)return;
	if(grthreatened[g1] || grthreatened[g2])return;
	if(grcolor[g4] == 1 - color && 
		gralive[g4] <= SMOTHERED && grarmy[g4] != grarmy[g2]){
		if(color == tm){
			cutval = cut_val(g4,g2);
			fire_strat_rule(s5,CUT_KNIGHT_WATARI,cutval,NOSQUARE,NOT_URGENT);
			}
		else {
			cutval = conn_val(g4,g2);
			if(!grthreatened[g4])
				cutval += def_val(grarmy[g4]);
			cutval += def_val(grarmy[g2]);
			fire_strat_rule(s3,KNIGHT_WATARI,cutval,NOSQUARE,NOT_URGENT);
			}
		}
	if(color != tm){
		if(gralive[g1] > ALIVE && gralive[g2] <= ALIVE)
			atkv = atk_val(grarmy[g1]);
		else atkv = 0;
		fire_strat_rule(s3,TRY_KNIGHTS_MOVE,atkv,NOSQUARE,NOT_URGENT);
		}
	}



void p_monkey_jump(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,s6,s7,g1,g2,g4,cutval;
	s5 = s + shapes[sh].critical[4];
	s6 = s5 + s5 - s2;
	s7 = s1 + s5-s2;
	g1 = board[s1];
	g2 = board[s2];
	g4 = board[s4];
	if(grlibs[g2] < 4)return;
	if(grthreatened[g2] || grthreatened[g1])return;
	if(gralive[g1] > WEAK || gralive[g2] > WEAK)return;
	if(WHITETOMOVE){
		if(grcolor[g4] == 1 - color && 
		   gralive[g4] <= SMOTHERED && grarmy[g4] != grarmy[g2]){
			cutval = conn_val(g4,g2);
			cutval += def_val(grarmy[g2]);
			if(!grthreatened[g4])
				cutval += def_val(grarmy[g4]);
			fire_strat_rule(s3,MONKEY_WATARI,cutval,NOSQUARE,NOT_URGENT);
			}
		if(gralive[g2] <= ALIVE){
			fire_strat_rule(s3,MONKEY_JUMP,atk_val(grarmy[g1]),NOSQUARE,NOT_URGENT);

			if( shapes[sh].pattno == 7 && board[s7] == NOGROUP)
				fire_strat_rule(s6,JUMP_UNDER_EDGE,atk_val(grarmy[g1]),NOSQUARE,NOT_URGENT);
			
			}
		}
	else {
		if(grcolor[lgr[s4]] == 1-color && 
		   grarmy[g4] != grarmy[g2]){
			cutval = cut_val(lgr[s4],g2);
			fire_strat_rule(s5,CUT_MONKEY_WATARI,cutval,NOSQUARE,NOT_URGENT);
			}
		else if(g4 == NOGROUP && lnbf[s4][tm] == 0)
			fire_strat_rule(s5,DEFEND_HARD_MONKEY,0,NOSQUARE,NOT_URGENT);
		fire_strat_rule(s5,DEFEND_MONKEY,0,NOSQUARE,NOT_URGENT);
		}
	}


void p_edge_hane(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,g2,atkval=0;
	if(BLACKTOMOVE)return;
	g1 = board[s1];
	g2 = board[s2];
	if(gralive[g1] > WEAK || gralive[g2] > WEAK)return;
	if(grlibs[g2] == 2 && gralive[g2] > ALIVE)atkval = atk_val(grarmy[g2]);
	fire_strat_rule(s3,EDGE_HANE,atkval,NOSQUARE,NOT_URGENT);
	}

void p_double_diag(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,cutval;
   	s5 = s + shapes[sh].critical[4];
	if(board[s3] == NOGROUP && lnbn[s5] == 4)return;
	if(lnbn[s5] < 3)return;
	if(BLACKTOMOVE){
	 	cutval = cut_val(board[s1],board[s2])/2;
		fire_strat_rule(s5,CUT_DBL_DIAG,cutval,NOSQUARE,NOT_URGENT);
		}
	else{
		if(grcolor[board[s4]] == tm && grarmy[board[s1]] == grarmy[board[s2]])
			fire_strat_rule(s5,DEFEND_DOUBLE_PEEP,0,NOSQUARE,NOT_URGENT);
		else {
			cutval = conn_val(board[s1],board[s2]);
			if(!moveispoteye(s5,grarmy[board[s1]]) && !moveispoteye(s5,grarmy[board[s2]])){
				cutval += def_val(grarmy[board[s1]]);
				cutval += def_val(grarmy[board[s2]]);
				}
			cutval /= 2;
			fire_strat_rule(s5,CONNECT_DBL_DIAG,cutval,NOSQUARE,NOT_URGENT);
			}
		}
	}



void p_edge_kosumi(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int g1,g2,val = 0;
	g1 = board[s1];
	g2 = board[s2];
	if(gralive[g1] > UNSETTLED || gralive[g2] > UNSETTLED)return;
	val = 200;
	fire_strat_rule(s3,EDGE_KOSUMI,val,NOSQUARE,NOT_URGENT);
	}


int hanelibs[] = { 0,0,200,100 };

void p_hane(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,s7,s8,s9,g1,g2,val,urg;
	s5 = s + shapes[sh].critical[4];
	s8 = s2+s2-s1;
	s9 = s1+s1-s2;
	s7 = s8+s4-s1;
	g1 = board[s1];
	g2 = board[s2];
	urg = inlist(grarmy[g1],&urgdefarmies);
	if(gralive[g1] > WEAK || gralive[g2] > WEAK)return;
	if(grthreatened[g1] || grthreatened[g2])return;
	if(board[s3] != NOGROUP && grlibs[board[s3]] > 1 || 
		board[s4] != NOGROUP && grlibs[board[s4]] > 1){
		fire_strat_rule(s5,PROTECTED_HANE,0,NOSQUARE,urg);
		}
	else if(grlibs[g1] < 3 && grlibs[g2] > 2)return;
	else if(grlibs[g1] < 5 && grcolor[board[s7]] == grcolor[g1])
		fire_strat_rule(s4,CONSIDER_ONE_PT_JUMP,0,NOSQUARE,NOT_URGENT);
	if(board[s3] == NOGROUP && board[s4] == NOGROUP &&
		(grcolor[board[s7]] == tm || lnbf[s7][tm] != 0 && lnbf[s7][1-tm] == 0))
		fire_strat_rule(s5,HANE_DEF_TERR,100,NOSQUARE,urg);
	else if(edge[s5] < 4 && edge[s5] < edge[s2])
		fire_strat_rule(s5,HANE_DEF_TERR,100,NOSQUARE,urg);
	fire_strat_rule(s5,HANE,0,NOSQUARE,urg);
	if(board[s9] == board[s1] && ld[s8] < 4 && grsize[g2] == 1)
		fire_strat_rule(s5,HANE_EXTEND_WALL,0,NOSQUARE,NOT_URGENT);
	if(gralive[g2] > ALIVE && gralive[g2] <= WEAK){
		val = 100;
/*		if(groupcanrunhere(g2,s5))val += atk_val(grarmy[g2]); */
		fire_strat_rule(s5,HANE_ATTACK_GROUP,val,NOSQUARE,NOT_URGENT);
		}
	if(grlibs[g2] < 4)
		fire_strat_rule(s5,HANE_TAKES_LIBS,hanelibs[grlibs[g2]],NOSQUARE,NOT_URGENT);
	}

int hanelibval[] = {
0,100,200,150,100 };

void p_stophane(int s,int sh,int color,int s1,int s2,int s3,int s4){
	int s5,s6,s7,s8,val,g1,g2;
	s5 = s + shapes[sh].critical[4];
	s6 = s1 + s5-s2;
	s8 = s2+s2-s1;
	s7 = s8+s4-s1;
	g1 = board[s1];
	g2 = board[s2];
	if(gralive[g1] > WEAK || gralive[g2] > WEAK)return;
	if(board[s3] != NOGROUP || board[s4] != NOGROUP && grlibs[g1] > 2){
		val = 0;
		if(gralive[g1] > ALIVE)
			val = 100;
		if(grlibs[g2] == 3 && grsize[g2] == 1)val += 300;
		fire_strat_rule(s5,PREVENT_GOOD_HANE,val,NOSQUARE,NOT_URGENT);
		}
	if(edge[s5] > edge[s6] && edge[s5] <= 4){
		val = edge[s6]*50;
		if(grcolor[board[s7]] == 1-tm || grcolor[board[s8]] == 1-tm)
			val = 0;
		fire_strat_rule(s5,NOT_UP_FROM_EDGE,val,NOSQUARE,NOT_URGENT);
		if(gralive[g1] > ALIVE && gralive[g1] <= WEAK)
			fire_strat_rule(s5,ATTACK_PREVENT_HANE_UP,
				0,NOSQUARE,NOT_URGENT);
		}
	if(grlibs[g2] < 5){
		val = hanelibval[grlibs[g2]];
		val += atk_val(grarmy[g1]);
		if(grcolor[board[s7]] == 1-tm || grcolor[board[s8]] == 1-tm)val = 0;
		if(grcolor[board[s8]] != tm){
			fire_strat_rule(s5,PREVENT_HANE_FEW_LIBS,val,NOSQUARE,NOT_URGENT);
			if(edge[s5] < edge[s6] && edge[s5] <= 4)
				fire_strat_rule(s5,PREVENT_HANE_TO_EDGE,0,NOSQUARE,NOT_URGENT);
			}
		else if(grcolor[board[s7]] != 1-tm)
			fire_strat_rule(s5+s5-s2,PREVENT_HANE_FEW_LIBS,val,NOSQUARE,NOT_URGENT);

		}
	if(gralive[g2] > ALIVE && groupcanrunhere(g2,s5)){
		if(!grthreatened[g2])val = def_val(grarmy[g2]);
		else val = 0;
		fire_strat_rule(s5,RUN_PREVENT_HANE,val,NOSQUARE,NOT_URGENT);
		if(grcolor[board[s8]] == grcolor[g2])
			fire_strat_rule(s5+s5-s2,RUN_PREVENT_HANE,val,NOSQUARE,NOT_URGENT);
		}
	}

/* TRUE if group g can run at s */

int groupcanrunhere(int g,int s){
	int i,ptr;
	for(i = 0; i < NUMRUN; ++i)
		for(ptr = armyrun[grarmy[g]][i]; ptr != EOL; ptr = links[ptr])
			if(list[ptr] == s)return(TRUE);
	return(FALSE);
	}

/* opponent is threatening to push thru your wall so block it 
 * or you are threatening to push thru so do it 
 */
  
void p_pushthru(int sp,int sh,int color,int s1,int s2,int s3,int s4){
   int sum,g3,g2,g1,s,ptr;
   g1 = board[s1];

   if(edge[s4] == 1)return; /* let edge cut handle this one */
   if(WHITETOMOVE && grthreatened[g1])return;  
        /* save group before worrying about pushing */
   if(gralive[g1] > WEAK){
	return;
	}

   g3 = board[s3];
   g2 = board[s2];

   if(gralive[g2] == DEAD || gralive[g3] == DEAD)return;



   if(gralive[g3] <= SMOTHERED && gralive[g2] <= SMOTHERED &&
	grarmy[g3] != grarmy[g2]){
      	if(tm != grcolor[g3]){
		fire_strat_rule(s4,BLOCK_CUTS,0,NOSQUARE,NOT_URGENT);
		}
	else{
		fire_strat_rule(s4,BLOCK_CONNECTS,0,NOSQUARE,NOT_URGENT);
		}
      	}


   sum = 0;
   for(ptr = nblbp[s4]; ptr != EOL; ptr = links[ptr]){
	s = list[ptr];
	if(ld[s] == NEUTRALLD && gralive[lgr[s]] <= SMOTHERED){
		sum = 100;
		fire_strat_rule(s4,NOT_PUSH_THRU,0,NOSQUARE,NOT_URGENT);
		break;
		}
   	if(board[s] == NOGROUP){
      		if(ltr1[s] == 0)sum += 25*lnbn[s];
      		else sum += 25*ltr1[s];
		}
      	}


   fire_strat_rule(s4,BLOCK_PUSH_THRU,sum,NOSQUARE,NOT_URGENT);

   }


void p_connect_hane(int s,int p,int color,int s1,int s2,int s3,int s4){
	int cutpoint,g1,g2,value,s5;
	s5 = s + shapes[p].critical[4];
	g1 = board[s1];
	g2 = board[s2];

	if(gralive[board[s1]] == DEAD || gralive[board[s2]] == DEAD)return;
	if(grarmy[board[s1]] == grarmy[board[s2]])return;
	cutpoint = s3;

	value = conn_val(g1,g2);
	if(grthreatened[g1] || grthreatened[g2])
		value = 0;
	fire_strat_rule(cutpoint,CONNECT_HANE,value,NOSQUARE,NOT_URGENT);
	fire_strat_rule(cutpoint,CONNECT_GR,0,NOSQUARE,NOT_URGENT); /* solid connection is best */
	if(board[s4] != NOGROUP || board[s5] != NOGROUP)return;

	if(grsize[board[s1]] == 1){
		value += 75;
		if(grsize[board[s2]] == 1 && grlibs[board[s2]] <= 2)
			value -= 50;
		}
	if(grsize[board[s2]] == 1){
		value += 75;
		if(grsize[board[s1]] == 1 && grlibs[board[s1]] <= 2)
			value -= 50;
		}
	if(grthreatened[g1] || grthreatened[g2])
		value = 0;
	fire_strat_rule(s4,CONNECT_HANE,value,NOSQUARE,NOT_URGENT);
	fire_strat_rule(s5,CONNECT_HANE,value,NOSQUARE,NOT_URGENT);
	}


int numshapes;

/* initialize the shapes data structure */

void initshapes(void){
	int pattno,orient,startshape,maxo;
	numshapes = 0;
	nextpoint = 0;
	for(pattno = 0; pattno < NUMPATS; ++pattno){
		startshape = numshapes;
		maxo = 8;
		if(patterns[pattno].sym && patterns[pattno].where == EDGE)
			maxo = 4;
		for(orient = 0; orient < maxo; ++orient){
			copyshape(pattno,orient,numshapes);
			if(patterns[pattno].sym && 
			   (patterns[pattno].where == ANYWHERE)){
				if(!newshape(numshapes,startshape))
					continue;
				}
			nextpoint += shapes[numshapes].numpoints;
			++numshapes;
			}
		}
	for(pattno = 0; pattno < numshapes; ++pattno)sortshape(pattno);
	}


/* sort the points in shape so will match faster */

void sortshape(int shp){
	int num,ptr,i,last,tmp,t;
	num = shapes[shp].numpoints;
	ptr = shapes[shp].startpoint;
	last = ptr + num;
	for(i = MAXPATVAL; i >= MINSORTVAL; --i){
		for(tmp = ptr; tmp < last; ++tmp){
			if(values[tmp] == i){
				values[tmp] = values[ptr];
				values[ptr] = i;
				t = points[ptr];
				points[ptr++] = points[tmp];
				points[tmp] = t;
				}
			}
		}
	}


/* checks for duplicate shapes due to symetry */

int newshape(int shapeno,int start){
	int i;
	for(i = start; i < shapeno; ++i){
		if(sameshape(i,shapeno))return(FALSE);
		}
	return(TRUE);
	}

/* return true if shapes i and shape are the same */

int sameshape(int i,int shape){
	int j,k,flag,pi,ps;
	pi = shapes[i].startpoint;
	for(j = 0; j < shapes[i].numpoints; ++j){
		flag = FALSE;
		ps = shapes[shape].startpoint;
		for(k = 0; k < shapes[shape].numpoints; ++k){
			if(points[ps] == points[pi]){
				flag = TRUE;
				if(values[pi] != values[ps])return(FALSE);
				break;
				}
			++ps;
			}
		++pi;
		if(!flag)return(FALSE);
		}
	return(TRUE);
	}

int whichedge[8] = {
BOTTOM,RIGHT,TOP,LEFT,BOTTOM,RIGHT,TOP,LEFT };


/* copy the pattern at pattno to the shape at shapeno.  Change orientation
 * and color.
 */

void copyshape(int pattno,int orient,int shapeno){
	int index,x,y,xs,ys,pptr;
	pptr = nextpoint;
	shapes[shapeno].pattno = pattno;
	shapes[shapeno].startpoint = pptr;
	if(patterns[pattno].where == ANYWHERE)
		shapes[shapeno].where = ANYWHERE;
	else
		shapes[shapeno].where = whichedge[orient];
/*	shapes[shapeno].seq = patterns[pattno].sequence;*/
	ys = PATYSIZE;
	xs = PATXSIZE;
	for(y = 0; y < PATYSIZE; ++y)
		if(patterns[pattno].pat[y][0] == 0){
			ys = y;
			break;
			}
	for(x = 0; x < PATXSIZE; ++x)
		if(patterns[pattno].pat[0][x] == 0){
			xs = x;
			break;
			}
	for(y = 0; y < ys; ++y){
		for(x = 0; x < xs; ++x){
			if(patterns[pattno].pat[y][x] == DC && patterns[pattno].critical[y][x] == 0)
				continue;
			switch(orient){
				case 0:
				index = x + y*boardsize;
				shapes[shapeno].xsize = xs;
				shapes[shapeno].ysize = ys;
				break;
				case 1:
				index = y + x*boardsize;
				shapes[shapeno].xsize = ys;
				shapes[shapeno].ysize = xs;
				break;
				case 4:
				index = xs-x-1 + y*boardsize;
				shapes[shapeno].xsize = xs;
				shapes[shapeno].ysize = ys;
				break;
				case 3:
				index = ys-y-1 + x*boardsize;
				shapes[shapeno].xsize = ys;
				shapes[shapeno].ysize = xs;
				break;
				case 2:
				index = x + (ys-y-1)*boardsize;
				shapes[shapeno].xsize = xs;
				shapes[shapeno].ysize = ys;
				break;
				case 5:
				index = y + (xs-x-1)*boardsize;
				shapes[shapeno].xsize = ys;
				shapes[shapeno].ysize = xs;
				break;
				case 6:
				index = xs-x-1 + (ys-y-1)*boardsize;
				shapes[shapeno].xsize = xs;
				shapes[shapeno].ysize = ys;
				break;
				case 7:
				index = ys-y-1 + (xs-x-1)*boardsize;
				shapes[shapeno].xsize = ys;
				shapes[shapeno].ysize = xs;
				break;
			        }
			if (patterns[pattno].pat[y][x] != DC) {
			        // sm: there is a bug here: during normal operation,
			        // the 'points' and 'values' arrays are accessed beyond
			        // their ends with arguments "5 4"; I simply disable
			        // the access when we're beyond the end on the assumption
			        // we won't later be using it
			        if (pptr < NUMPOINTS) {
                                  points[pptr] = index;
                                  values[pptr++] =
                                          patterns[pattno].pat[y][x];
                                }
                                else {
                                  fprintf(stderr, "array bug: index is %d (vs %d)\n", pptr, NUMPOINTS);
                                }
			}
		    if(patterns[pattno].critical[y][x] != 0)
				shapes[shapeno].critical[patterns[pattno].critical[y][x]-1] = 
					index;
			}
		}
	shapes[shapeno].numpoints = pptr-shapes[shapeno].startpoint;
	}



/* find the shapes on the board 
 * fsqr is the upper left corner and lsqr is the lower right of
 * a rectangle enclosing all the points where stones were added or
 * deleted since the last call to findshapes.
 */


void findshapes(int fsqr,int lsqr){
	int s,sh;
	int top; /* start at this point */
	int bot; /* stop when get past this point */
	int width; /* width of rectangle */
	int right; /* right edge of rectangle */
	int color, point,t,b,l,r,left,up;
	t = b = l = r = TRUE;  /* do all patterns at top, bottom, etc */
	for(sh = 0; sh < numshapes; ++sh){
   	   if(shapes[sh].xsize > boardsize || shapes[sh].ysize > boardsize)
		   continue;
	   bot = lsqr;
	   left = xval[fsqr] - shapes[sh].xsize + 1;
	   if(left < 0)left = 0;
	   up = yval[fsqr] - shapes[sh].ysize + 1;
	   if(up < 0)up = 0;
	   top = up * boardsize + left;

	   if(xval[bot] + shapes[sh].xsize > boardsize)
		   bot -= xval[bot] + shapes[sh].xsize - boardsize;
	   if(yval[bot] + shapes[sh].ysize > boardsize)
		   bot -= boardsize * (yval[bot] + shapes[sh].ysize - boardsize);
	   right = xval[bot];
	   width = right - xval[top] + 1;
	   t = yval[top] == 0;
	   b = yval[bot] == boardsize-shapes[sh].ysize;
	   l = xval[top] == 0;
	   r = xval[bot] == boardsize-shapes[sh].xsize;
	   color = vclr[values[shapes[sh].startpoint]];
	   point = points[shapes[sh].startpoint];
	   switch(shapes[sh].where){
		case ANYWHERE:
		for(s = top; s <= bot; ++s){
			if(xval[s] > right)s += boardsize - width;
			if(grcolor[board[s+point]] == color && match(sh,s))
				addlist(sh,&shapebrd[s]);
			else if(grcolor[board[s+point]] == 1-color && 
				match2(sh,s))
				addlist(sh,&shapebrd[s]);
			else if(shapebrd[s] != EOL)
				dellist(sh,&shapebrd[s]);
			}
		break;
		case TOP:
		if(t)for(s = top; s < top + width; ++s)
			if(grcolor[board[s+point]] == color && match(sh,s))
				addlist(sh,&shapebrd[s]);
			else if(grcolor[board[s+point]] == 1-color && 
				match2(sh,s))
				addlist(sh,&shapebrd[s]);
			else if(shapebrd[s] != EOL)
				dellist(sh,&shapebrd[s]);
		break;
		case BOTTOM:
		if(b)for(s = bot - width + 1; s <= bot; ++s){
			if(grcolor[board[s+point]] == color && match(sh,s))
				addlist(sh,&shapebrd[s]);
			else if(grcolor[board[s+point]] == 1-color && 
				match2(sh,s))
				addlist(sh,&shapebrd[s]);
			else if(shapebrd[s] != EOL)
				dellist(sh,&shapebrd[s]);
			}
		break;
		case LEFT:
		if(l)for(s = top; s <= bot; s += boardsize){
			if(grcolor[board[s+point]] == color && match(sh,s))
				addlist(sh,&shapebrd[s]);
			else if(grcolor[board[s+point]] == 1-color && 
				match2(sh,s))
				addlist(sh,&shapebrd[s]);
			else if(shapebrd[s] != EOL)
				dellist(sh,&shapebrd[s]);
			}
		break;
		case RIGHT:
		if(r)for(s = bot; s >= top; s -= boardsize){
			if(grcolor[board[s+point]] == color && match(sh,s))
				addlist(sh,&shapebrd[s]);
			else if(grcolor[board[s+point]] == 1-color && 
				match2(sh,s))
				addlist(sh,&shapebrd[s]);
			else if(shapebrd[s] != EOL)
				dellist(sh,&shapebrd[s]);
			}
		break;
		}
	  }
        }

/* does shape sh exist at s */
/* first point has already been checked and matches */

int match(int sh,int s){
	int i,pptr;
	pptr = shapes[sh].startpoint+1;
	for(i = 1; i < shapes[sh].numpoints; ++i){
		switch(values[pptr]){
			case DC:
				break;
			case WB:
				if(grcolor[board[s+points[pptr]]] == NOCOLOR)return(FALSE);
				break;
			case EM:
				if(grcolor[board[s+points[pptr]]] != NOCOLOR)return(FALSE);
				break;
			case WH:
				if(grcolor[board[s+points[pptr]]] != WHITE)return(FALSE);
				break;
			case WE:
				if(grcolor[board[s+points[pptr]]] == BLACK)return(FALSE);
				break;
			case BL:
				if(grcolor[board[s+points[pptr]]] != BLACK)return(FALSE);
				break;
			case BE:
				if(grcolor[board[s+points[pptr]]] == WHITE)return(FALSE);
				break;
			}
		++pptr;
		}

	return(TRUE);
	}


/* does shape sh exist at s */
/* first point has already been checked and matches */

int match2(int sh,int s){
	int i,pptr;
	pptr = shapes[sh].startpoint+1;
	for(i = 1; i < shapes[sh].numpoints; ++i){
		switch(values[pptr]){
			case DC:
				break;
			case WB:
				if(grcolor[board[s+points[pptr]]] == NOCOLOR)return(FALSE);
				break;
			case EM:
				if(grcolor[board[s+points[pptr]]] != NOCOLOR)return(FALSE);
				break;
			case BL:
				if(grcolor[board[s+points[pptr]]] != WHITE)return(FALSE);
				break;
			case BE:
				if(grcolor[board[s+points[pptr]]] == BLACK)return(FALSE);
				break;
			case WH:
				if(grcolor[board[s+points[pptr]]] != BLACK)return(FALSE);
				break;
			case WE:
				if(grcolor[board[s+points[pptr]]] == WHITE)return(FALSE);
				break;
			}
		++pptr;
		}

	return(TRUE);
	}


void evalshapes(void){
	int i,ptr;
	for(i = firstsquare; i < lastsquare; i++){
		for(ptr = shapebrd[i]; ptr != EOL; ptr = links[ptr]){
			callfunc(i,list[ptr]);
			}
		}
	}


