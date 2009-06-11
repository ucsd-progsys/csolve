/* Copyright 1993 David Fotland.
 * Permission granted to Spec to use and distribute as a computer benchamrk
 * permission is not granted to use or modify this code for any
 * purpose other than benchmarking or computer system performance tuning.
 *
 * This program is available (with a user interface) for:
 * IBM-PC/DOS from Ishi Press Inc San Jose CA (408)944-9900
 * Penpoint from PenGames San Jose Ca (408)985-1236
 */

# include "g2protos.h"

# define printw printf
# define refresh() fflush(stdout)


struct strategy {
	int reason;      /* reason to try this move (rule #) */
	int value;       /* extra strategic value for this move */
	int param;       /* square in group to check aliveness after lookahead */
	                 /* if square is enemy stone, it should be deader */
                         /* NOGROUP means use the group on the square moved to */
	int goodrule;    /* TRUE if this rule is satisfied after lookahead */
	int urgent;      /* TRUE if this rule is urgent */
	};

struct rulestruct {
	char * NTS reason;	/* reason for rule */
	int value;	/* value of rule (added after lookahead) */
	int guess;      /* guessed value of rule (used to select moes to try) */
	int weakest;	/* weakest group this rule applies to */
	int attack;   /* TRUE if rule is for attacking group */
	};


# define PATYSIZE 6
# define PATXSIZE 5
# define MAXSIZE 6


struct potential {
	int pot_where; /* square for undercut */
	               /* connection number for connect */
	               /* eye record number for vital */
                       /* group number for threat */
	               /* liberty for extend */
	int pot_val;  /* 8 per eye, for undercut is amount of territory */
	int pot_type;  /* extend, vital, connect, threat, undercut */
	};

struct pattstruct { /* pattern assuming white to move */
/*	char *sequence;   sequence of moves to expect here */
	int pat[PATYSIZE][PATXSIZE];
 	int critical[PATYSIZE][PATXSIZE];
	int where;  /* EDGE or ANYWHERE */
	int sym;    /* TRUE if symettric pattern */
	};


struct shapestruct {
/* 	char *seq; */
	int startpoint;         /* first point in shape */
	int pattno;		/* pattern number */
	int numpoints;		/* number of points in shape */
	int xsize,ysize;	/* size of pattern in x and y */
	int where;		/* where to look for pattern */
	int critical[MAXSIZE];  /* interesting points by color to move */
	};

struct jos_struct {
	int more;     /* pointer sibling */
	int next;     /* pointer for child */
	int xy;      /* 4 bits x, 4 bits y */
	int flag;   /* 0x80 change jreflect for continuation
			 0x40 make symetric
			 0x20 color reverse
			 0x10 Move is same color as first move in corner
			 0-15 type
			 */
	int lib2val;   /* used in format conversion */
	};


/* the following group of defines is safe to change since no harm comes
 * except stupidness if they are two small
 */



# define NUMMOVES 800
# define MAXMOVE 760
# define MAXLADDER (MAXMOVE-20)
# define NUMGROUPS 750
# define NUMSTRATS 500
# define NUMPLY 90
# define NUMPOTENTIAL 650
# define NUMARMIES 450

/* program will crash if NUMARMIES too small */

/* NUMEYERECS must be less than 128 */
# define NUMEYERECS 127
# define NUMCONNS 450
# define NUMPCLS (NUMMOVES)
# define NUMNBLB 1368
# define NUMLIST (NUMNBLB+NUMEYERECS+NUMARMIES+NUMCONNS+11200)
# define NUMSQUARES (29*29)
# define NUMRUN 8

# define NUMJOSBYTES 12384

# define ERROR 0xf000
# define EOL NUMLIST-1
# define PASS -10
# define NOCONN -1
# define NOSQUARE NUMSQUARES
# define NOGROUP NUMGROUPS-1
# define NOARMY NUMARMIES-1
# define NOCORNER 4
# define NOEDGE 20
# define BLACK 0
# define WHITE 1
# define NOCOLOR 2
# define NOLD 99
# define NEUTRALLD 1
# define BIGNUM 32767

/* for cntype[] */

# define CN_UNKNOWN 0
# define CN_ONEPOINTJUMP 1
# define CN_KNIGHTSMOVE 2
# define CN_TWOPOINTJUMP 3
# define CN_HANE 4
# define CN_BAMBOOJOINT 5
# define CN_DIAGONAL 6
# define CN_MULTIPLE 7
# define CN_ONESIDEDEAD 8
# define CN_THREAT 9
# define CN_HALFKNIGHT 10
# define CN_THREEPOINTJUMP 11
# define CN_LARGEKNIGHT 12
# define CN_HALFLARGEKNIGHT 13

/* for gralive[] */

# define DEAD 25
# define SMOTHERED 24
# define WEAK_GROUP 23
# define VERY_WEAK 23
# define LOSE_SEMEAI 20
# define WEAK 19
# define WEAK_POTENTIAL 18
# define WEAK_LIMP 17
# define WEAK_RUN 16
# define WEAK_RUN_EYE 15
# define UNSETTLED 14
# define SEMEAI 14
# define UNSETTLED_DEAD 13
# define UNSETTLED_RUN 12
# define UNSETTLED_THREATENED 11
# define ALIVE 10
# define WINS_SEMEAI 10
# define MIAI 9
# define STRONG_MIAI 8
# define SEKI 7
# define VERY_ALIVE 6
# define HAS_TWO_EYES 2
# define NUM_LIVENESS_STATES DEAD+1

# define EASY_RUN 12

/* for potential */

# define EXTEND 0
# define VITAL 1
# define THREAT 2
# define CONNECT 3
# define UNDERCUT 4
# define NOPOT 5

# define BAD_MOVE -20000

# define HP150 0
# define HP239X 2
# define HP264X 1

# define UP 0
# define LEFT 1
# define RIGHT 2
# define DOWN 3

/* for eyetype[] */

# define NOEYE 0
# define ONEPOINTEYE 1
# define TWOPOINTEYE 2
# define DEADEYE 3
# define THRTEYE 4
# define BIGEYE 5
# define LINEEYE 6
# define FOURPTBLOCKEYE 7
# define OPENLINEEYE 8
# define CORNEREYE 9

/* for josekis[].jflag[] */

# define NORM 0
# define URGT 1
# define BAD 2
# define COMP 3
# define THFR 4
# define FRFR 5
# define SHIM 6
# define FOLL 7
# define TRIK 8
# define IGNR 9
# define KAKA 10
# define THTH 11
# define FRKA 12
# define PINC 13
# define HIGH 14

# define ATTACK 1
# define DEFEND 2

# define FUSEKI 0
# define MIDDLE 1
# define ENDGAME 2

/* for cnprot[] */

# define PROTCONN(x) (cnprot[x] >= AJI_CONNECT)

/* cuttable connections */
/* can't connect must be zero for canconnlink */
/* any nonzero value must be able to connect */

# define CANT_CONNECT 0
# define CAN_CONNECT 1
# define SHARED_CONNECT 2

/* uncuttable connections */

# define AJI_CONNECT 3
# define SOLID_CONNECT 4

# ifndef TRUE
# define FALSE 0
# define TRUE 1
# endif

# define NO 0
# define YES 1
# define MAYBE 2
  
# define MAXRTVAL 100
	/* divisor for adjusting radiated territory when done 
           MAXRTVAL is one point */
# define URGENT 1
# define NOT_URGENT 0

#define NUM_RULES 263
