/* Copyright 1993 David Fotland.
 * Permission granted to Spec to use and distribute as a computer benchamrk
 * permission is not granted to use or modify this code for any
 * purpose other than benchmarking or computer system performance tuning.
 *
 * This program is available (with a user interface) for:
 * IBM-PC/DOS from Ishi Press Inc San Jose CA (408)944-9900
 * Penpoint from PenGames San Jose Ca (408)985-1236
 */


# include <stdlib.h>
# include <string.h>
# include <stdio.h>
# include <ctype.h>
# include "g2def.h"

extern int highlighted,hcpoints;

int gameover = FALSE;           /* just printed out "game over" */
int maxmove;		/* move number for computer to stop playing */


/* data structures attatched to each square */

/* constant data */
int edge[NUMSQUARES+1];	/* distance to closest edge, 0 for corner */
int edge2[NUMSQUARES+1];	/* dist to 2nd closest edge */
int fdir[NUMSQUARES];   /* dist from edge, dirs for squares */
int xval[NUMSQUARES];	/* x value for square starting at zero */
int yval[NUMSQUARES];	/* y value for square starting at zero */

int board[NUMSQUARES+1];    /* group number of the piece on that square */
				/* +1 for NOSQUARE */
int shapebrd[NUMSQUARES];	/* lists of shapes matched at each square */
int ld[NUMSQUARES];        /* neighboring pieces info */
			   /* NOLD - no stones adjacent */
			   /* NEUTRALLD - two colors of stone adjacent */
			   /* 2 for nbr, 1 for nbr of empty nbr */
int lgr[NUMSQUARES];       /* group controlling this square */
int lnbf[NUMSQUARES][2];   /* number of full neighboring squares by color */
int lnbn[NUMSQUARES];      /* number of empty neighboring squares */
int nblbp[NUMSQUARES];	   /* lists of empty neighboring squares */

int ltr1[NUMSQUARES];      /* amount of territory from this square */
int ltr2[NUMSQUARES];      /* value of edge territory with ltrgd == 0 */
int ltrgd[NUMSQUARES];		/* flag if territory is good 
				 * 0-good territory
				 * 1-off to side of stone
				 * 2-neutral or threatened 
				 * 3-under stone next to enemy stone
				 * 4 under stone and undercut one side
				 * 5 off to side and undercut
				 * 8 undercut 2 sides 
				 * 9 off to side and undercut 2 sides
				 */
int eyerec[NUMSQUARES];		/* eye record number for this eye 0=none */
				/* corresponds to eyeptr */
				/* a point on board can only be part of one eye*/
                                /* only ld values >= 6 have eyerecs */
int eyevitrec[NUMSQUARES];	/* list of eye records that have this vital point */
int eyesdiags[NUMSQUARES];  /* lists of points with eyes diagonal from this square */

struct strategy strat[NUMSTRATS];  /* strategy records */
int urgent[NUMSQUARES];    /* count of how many urgent reasons there are */
                            /* incremented in fire_strat_rule, */
                            /* decremented in stval if rule doesn't apply */
int stratreasons[NUMSQUARES];    /* lists of strategy records */
int nextstrat;           /* next strategy record to use */

int stratguess[NUMSQUARES];    /* guess for move value of move at this point */
int strattotal[NUMSQUARES];    /* bonus strategic value total for this point */
int stratgoodreasons[NUMSQUARES];  /* TRUE if there is a good reason for move*/

int scoreval[NUMSQUARES];	/* save lookahead score */

int rterv[NUMSQUARES][2];	/* radiated territory value for square by color */

int terv[NUMSQUARES];		/* territory value for each square */
int ldrflag[NUMSQUARES];       /* ladder lists for moves */
int cnbrd[NUMSQUARES];         /* lists of connection records by point */
int lkbrd[NUMSQUARES];		/* lists of conn records for linkages */
                                /*    (2 pt jump and knight move ) */
int llbrd[NUMSQUARES];          /* lists of conn records for long linkages */
                                /* (3 pt jump and large knight move) */
int sqrbrd[NUMSQUARES][4];	/* nearest occ sqr in each direction */
				/* (or NOSQUARE). */
int dstbrd[NUMSQUARES][4];	/* dist to nearest occ sqr in each dir */
				/* number of empty points (0-3). */

/* eye record data structures */

int eyefreelist;	/* list of free eye records */
int eyetype[NUMEYERECS];	/* type of eye */
				/* NOEYE, ONEPOINTEYE, TWOPOINTEYE, DEADEYE, 
				   THRTEYE, BIGEYE */
int eyeptr[NUMEYERECS];		/* list of squares in this eye */
int eyevital[NUMEYERECS];	/* list of vital points for this eye */
				/* a vital point is where an opponent can
				 * play to eliminate the potential and
				 * reduce the value of the eye to eyeval 
				 * to increase the value of an eye from eyeval
				 * to eyepot you must control the vital
				 * point.  If the vital point is on a group
				 * then this group is threatened and must
				 * be settled to settle the eye.  An eye can
				 * have more than one vital point */
int eyeval[NUMEYERECS];		/* number of eyes if opponent moves first */
int eyepot[NUMEYERECS];		/* number of eyes if I move first */
int eyemin[NUMEYERECS];		/* min number of eyes if don't respond to
					opponents move */

int armyfreelist;          /* list of free army records */
int charmy;                /* list of armies which changed groups */
 			/* actually squares of stone in army */
int chalive;               /* list of armies whose aliveness changed */

struct potential pots[NUMPOTENTIAL];  /* potentials for armies */

/* information kept per army */

int armypot[NUMARMIES];      /* list of potential records per army */
int armygroups[NUMARMIES];   /* list of groups in army */
int armydeadgroups[NUMARMIES]; /* list of dead groups in army */
int armysize[NUMARMIES];   /* number of stones in army */
int armyalive[NUMARMIES];  /* aliveness of army */
int armyeyes[NUMARMIES];   /* number of eyes of army 8 = 1 eye */
			   /* counts point eyes and dead groups */
int armyterr[NUMARMIES];   /* territory held by army (eyes don't count) */
			/* note, limit this to 128! */
int armyrun[NUMARMIES][NUMRUN];  /* lists of liberties where can run */
                        /* 0-5 no enemy stone nearby, wide open */
			/* 0 - friendly influence only */
			/* 1 - Wide open - no influence, no enemy stones */
			/* 2 - friendly and unfriendly influence  more friendly*/
			/* 3 - friendly and unfriendly influence, more unfr.*/
                        /* 4 - unfriendly only, but running away from */
			/* 5 - Unfriendly influence only, running toward enemy */
                        /* 6 - hole can push thru for more liberties */
                        /* 7 - short of liberties - can't jump here */
int armyrn_pot[NUMARMIES];	/* potential eyes from running */
int armywk_pot[NUMARMIES];    /* potential eyes from neighboring weak groups */
int armyeyepotential[NUMARMIES]; /* total extra eye potential 16 = 1 eye */
int armylibs[NUMARMIES];   /* number of liberties for army */
			/* note: keep under 128! */
int armylbp[NUMARMIES];		/* army liberty list */
int armynbp[NUMARMIES];    /* list of neighboring armies for army (not always valid) */
                           /* includes touching armies and shared liberties */
int armyeyerecs[NUMARMIES]; 	/* list of eye records for this army */
int armyvitalpoints[NUMARMIES]; /* list of vital points for all eyes in army */
	/* these are places where enemy can play to eliminate potential */
	/* for more eyes */
int armyeyespace[NUMARMIES];    /* total eyespace for army */
				/* eyes + territory */

int xmax,xmin,ymax,ymin;  /* area to reevaluate edge territory */
int lookaheadflag;
int lookldr = EOL;	  /* points where ladders happened due to stone
			     being put down last eval */
int kosquare;             /* illegal move square for ko */
int handicap;		/* number of handicap stones */
int boardsize;		/* size of edge of board */
int boardsquare;	/* boardsize * boardsize */
int firstsquare;	/* index of first square on board */
int lastsquare;		/* firstsquare + boardsquare */
int pcls[NUMPCLS],pclsnext;   /* pieces changed since last eval */
int chgrp; /* list of groups with changed territory due to changed
				aliveness value */

int list[NUMLIST];
int links[NUMLIST];  /* lists values and links */

int freelist;			/* freelist head for list elements */

int terhd;			/* head of list of territory needing eval */

int scr;	/* total score for current position */
int tscr;	/* total territory score for liberties territory */
int pscr;	/* total score for pieces on board */
int rtscr;	/* total territory based on radiated influence */
int ltrscr;     /* total territory score for edge territory (ltrgd == 0)*/
int stscr;	/* total strategic score */
int thrscr;     /* total score for weak and unsettled groups on board */

int bstscr;
long timeused[2];  /* time by color */
extern int hcwhite[],hcblack[];
int eyelist;	/* list of points that are potential eyes */


int grldr[NUMGROUPS+NUMCONNS+NUMSQUARES];  /* ladder lists for gr,cn,li */
                        /* has list of points that impact a tactical search */
int cnchgd;             /* list of connections needing eval */
int sply[NUMPLY],eply[NUMPLY],scrply[NUMPLY];  /* tactical lookahead */
int nbply[NUMMOVES],lbply[NUMMOVES];  /* more tactical  (lists)*/
int noladder;           /* don't call ladder if true */
int cplay[2];           /* who plays this color */
int sc;
int evcolor;               /* color when last eval done */

			/* Josekis */
int jflag[4];		/* False if no joseki moves left in this corner */
                        /* 1 if still in joseki tree */
                        /* 2 if one move past joseki */
                        /* 3 or more if additional moves played past joseki */
int jreflect[4];	/* 0 - symetric corner, 1 - reflected, 2 - as shown */
int jptr2[4];           /* points in jlib2 to move just played by corner */
int jcolor[4];		/* color that moved first in corner */
                        /* never points at a tenuki */

/* move descriptions */  
int tm;   			/* side to move */
int msptr;			/* next move number.  move stack pointer*/
int mvs[NUMMOVES];		/* move square for each move */
int mvcapt[NUMMOVES];		/* list of groups captured by move */
int mvconn[NUMMOVES];		/* list of group connected by move */
int mvcolor[NUMMOVES];		/* color of move */
int mvnext[NUMMOVES];		/* square for next piece in group */
int kosave[NUMMOVES];		/* saved ko sqaure */
int atariflag;			/* current move is atari */


/* group data structure */
int grcolor[NUMGROUPS];	/* color of group (grcolor[NOGROUP] is NOCOLOR)*/
int grlibs[NUMGROUPS];	/* number of liberties */
int grpieces[NUMGROUPS];	/* move stack index of first piece in group */
int gralive[NUMGROUPS];	/* aliveness of group (2-25) */
			/* 25 - tactically captured unconditionally */
			/* 24 - presumed dead because surrounded without
			   any eyes */
			/* 23 - Temp used for weak groups undecided yet */
			/*    19-22 can't run, presumed dead */
			/* 22 - No eyespace or potential and nbrs all alive */
			/* 21 - probably dead some eye space or potential, nbrs alive*/
			/* 20 - in semeai loses */
			/* 15-19 - some running potential, weak */
			/*   (19 no running ability, weak nbrs and some eye potential)*/
			/*   (18 can't run, lots of eye space) */
			/*   (17 poor running ability) */
			/*   (16 can run away easily, no eyes) */
			/*   (15 can run away easily, one eye) */
			/*     needs two moves to live */
			/* 11-14 unsettled - can make two eyes if moves first*/
                        /*   (14 in a semeai. need to attack nbr to live) */
			/*   (13 surrounded, can live or die) */
			/*   (12 can live or run) */
			/*   (11 would be alive, but tactically threatened) */
                        /* 10 - Alive because wins semeai */
                        /* 9 - miai for barely space for two eyes (dangerous) */
                        /* 8 - miai for lots of eye space */
                        /* 7 - alive in seki */
			/* 6- barely room for two eyes (dangerous) */
			/* 5- barely room for two eyes and can get more */
			/* 4- eyes plus territory for 2 1/2 eyes */
			/* 3- eyes plus territory for 3 eyes */
			/* 2 - has two separate eyes */
int grthreatened[NUMGROUPS]; /* TRUE if group is tactically unsettled - */
			/* captured if opponent moves first */
int groldalive[NUMGROUPS]; /* aliveness of group before lookahead */
int groldthreatened[NUMGROUPS];
int grlv[NUMGROUPS];		/* does group exist */
int grsize[NUMGROUPS];	/* number of stones in group */
int grlbp[NUMGROUPS];   /* liberty lists */
int grcnp[NUMGROUPS];   /* connection record lists */
int grnbp[NUMGROUPS];   /* enemy neighbor lists */
int grarmy[NUMGROUPS];     /* army number for group */
int grdeadarmy[NUMGROUPS]; /* army number in which this is dead group */


int numpris[2];		/* number of prisoners for each color */


/* connection descripers */
int cnfreelist;            /* free connection records */
int cngr1[NUMCONNS],cngr2[NUMCONNS];	/* group numbers for connection */
int cncnum[NUMCONNS],cnlknum[NUMCONNS],cnllnum[NUMCONNS];	/* number of conns and links*/
int cnptr[NUMCONNS];  /* list of connection points (solid)*/
int cnlkptr[NUMCONNS]; /* list of connection points (linkages) */
int cnllptr[NUMCONNS];	/* list of connection points (long linkages) */
                          /* conn and link points */
int cnprot[NUMCONNS];		/* is connection unbreakable */
                                /* 0 - can't connect in one move */
                                    /* one side dead, or breakable link, etc */
                                /* 1 - can connect in one move */
                                /* 2 - conected except interacts with another
                                       connection */
                                /* 3 - connected with Aji */
                                /* 4 - connected solidly */
int cntype[NUMCONNS];		/* type of connection */

int problemflag = FALSE;	/* solve tesugi and life/death probs */
int maxgr,maxpc;
int playlevel,quicklevel;             /* playing level */
int mvmost;      	/* number of moves considered for ladder at each ply*/
int taclibs;            /* max liberties in a tactical fight */
int quicklibs;          /* max number of libs for quick capture */
int numnodes,nummoves;


int brddir[4] = { -19,-1,1,19 };  
  
int ldir[52] = {
   2,2,
   4,4,
   6,6,
   8,8,
   11,11,11,
   14,14,14,
   17,17,17,
   20,20,20,
   24,24,24,24, 
   28,28,28,28, 
   32,32,32,32, 
   36,36,36,36, 
   40,40,40,40, 
   44,44,44,44, 
   48,48,48,48, 
   52,52,52,52 }; 
  
int nbr[52];
int nbri[52] = { 
   1,19,
   -1,19, 
   1,-19, 
   -1,-19,
   -1,1,19, 
   -19,19,1,
   -19,19,-1, 
   -1,1,-19,
   1,19,-1,-19, 
   -1,19,1,-19, 
   1,-19,-1,19, 
   -1,-19,1,19, 
   -1,1,-19,19, 
   -19,19,-1,1, 
   -19,19,1,-1, 
   -1,1,19,-19   };

int ldiag[52] = {
   1,1,
   3,3,
   5,5,
   7,7,
   10,10,10,
   13,13,13,
   16,16,16,
   19,19,19,
   24,24,24,24, 
   28,28,28,28, 
   32,32,32,32, 
   36,36,36,36, 
   40,40,40,40, 
   44,44,44,44, 
   48,48,48,48, 
   52,52,52,52,
};

int diags[52];
int diagsi[52]  = {
	20, 0,
	18, 0,
	-18, 0,
	-20, 0,
	18, 20, 0,
	-18,20, 0,
	-20,18, 0,
	-20,-18, 0,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
	-20, -18, 18, 20,
};
  
  
char colorchar[] = { 'B', 'W', 'N' };

int tournflag;  /* use usenix tournament I/O */
int xtflag;  /* use x and usenix I/O for two person play */
int debugflag; /* true if debug command enabled */

char *NTS progname;
int highlightlast;  /* highlight last move */




int graph,hp150;
  
int timeflag;	/* flag and time for time control */
int chineseflag;	/* TRUE for chinese rules */
int takestones;      /* true if are taking enemy stones off board */

int urgdefarmies;  /* list of armies it is urgent to defend */
int phase;      /* phase of game, type of game.  */
int ahead;      /* how are we doing?  0 way behind, 1 behind, 2 about even,
		                      3 ahead, 4 way ahead */
                /* used to adjust strategic values */

int komi;   /* how much komi (in 50ths of points ) */


int main(int argc, char *argv[]){

    // sm: some documentation..
    if (argc < 3) {
      printf("usage: %s playlevel boardsize [position]\n"
             "  playlevel: integer in [1,100] determining computer\n"
             "             player's skill; 100 is best skill\n"
             "  boardsize: integer in [4,29] giving board size (board\n"
             "             is always square)\n"
             "  position:  a file that specifies an initial game\n"
             "             configuration (sm: format unknown)\n",
             argv[0]);
      return 0;
    }

    initinit(atoi(argv[1]), atoi(argv[2]));

    initbsizeconst();
    
    init(TRUE,TRUE);

    if(argc > 3)
	readfile(argv[3]);
    life();
    sc = getscore();

    do {
       do {
          getmove();
          }while(!check());  /* check legality and make move for real */

       }while(!gameover);
	printf("Game over\n");
    g2exit(0);
    }
 
void readfile(char *name){
	FILE *in;
	char buf[80],*p;
	int row;
	printf("Initial position from %s\n",name);
	in = fopen(name,"r");
	if(in == NULL){
		printf("Can't open input file.\n");
		g2exit(1);
		}
	while(1){
		if(fgets(buf,80,in) == NULL)break;
		p = buf;
		while(*p != 0 && (*p == ' ' || isdigit(*p)))++p;
		if(*p == 'W')mvcolor[msptr] = 1;
		else mvcolor[msptr] = 0;
		p++; p++;
		if(strncmp(p,"pass",4) == 0){
			mvs[msptr] = PASS;
			}
		else{
			mvs[msptr] = *p-'A';     /* not so portable? */
			if(*p > 'I')mvs[msptr]--;
			++p;
#ifdef CCURED
                        row = atoi(p);
#else                        
			sscanf(p,"%d",&row);
#endif                        
			row = boardsize-row;
			mvs[msptr] += row*boardsize;
			}
		check();  /* make the move */
		}
	fclose(in);
	}
  
int check(void)
 {
    int s,flag,c;
    s = mvs[msptr];
    c = mvcolor[msptr];
    flag = cplay[c] == 1;
    gameover = FALSE;
    if(s == PASS){
       if(msptr > 0 && mvs[msptr-1] == PASS){
	  if(chineseflag){
                  if(msptr > 1 && mvs[msptr-2] == PASS && mvs[msptr-1] == PASS){
                          turnoffcplay();
                          gameover = TRUE;
                          }
                  else {
                        outerror(0,"Pass");
                        }         
	          }
	  else {        
        	  gameover = TRUE;
                  turnoffcplay();
                  }
          }
       }
    else {
       if(s < firstsquare || s >= lastsquare){
		  outerror(0,"BAD MOVE");
          if(flag){
                  turnoffcplay();
                  outerror(1,"Computer generated illegal move!");
				  printf("s is %d",s);
                  }
          return(FALSE);
          }
       if(board[s] != NOGROUP){
	  outerror(0,"Occupied square");
          if(flag){
                  turnoffcplay();
                  outerror(1,"Computer generated illegal move!");
                  }
          return(FALSE);
          }
       if(s == kosquare){
	  outerror(0,"Illegal Ko Capture.");
          if(flag){
                  turnoffcplay();
                  outerror(1,"Computer generated illegal move!");
                  }
          return(FALSE);
          }
       }
    if(real_update(msptr,!(cplay[tm] == 0 && chineseflag))){
       	if(msptr >= MAXMOVE || maxgr >= NUMGROUPS-20){
          	outerror(0,"We can't finish this game");
		outerror(1,"I am out of memory for moves");
          	dndate(msptr);
		turnoffcplay();
		return(FALSE);
          	}
	printf("%3d %c ",msptr+1,colorchar[tm]);
	psqr(mvs[msptr]);
	printf("\n");
	++msptr;
	tm = 1 - tm;
/*	fixscrn(); */
	life();
	sc = getscore();
       	return(TRUE);
       	}
    dndate(msptr);
    outerror(0,"Illegal self capture.");
    if(flag){
            turnoffcplay();
            outerror(1,"Computer generated illegal move!");
            }
    return(FALSE);
    }
  
void getmove(void){
    int haveamove = FALSE;
    while(!haveamove){
	    if(cplay[tm] == 0){
			outerror(0,"this version no person play allowed\n");
			g2exit(2);
		    }
	    if(haveamove)break;
	    if(cplay[tm] == 1){
	            if(chineseflag && tm == WHITE && msptr < handicap*2-1){
	                    haveamove = TRUE;
	                    mvs[msptr] = PASS;
	                    mvcolor[msptr] = tm;
	                    }
		    else
		            haveamove = compmove();
		    if(haveamove && mvs[msptr] == PASS && 
		            chineseflag && !takestones){
		            takestones = TRUE;
		            haveamove = compmove();
		            }
		    if(!haveamove){
		            outerror(0,"Comp didn't give move");
		            }
		    }
	    }
   }
  
  
  
  
  
void init(int obdflag,int handicapflag){
   initvars();
   }
  

void g2exit(int i){
   exit(i);
   }

void fixplaylevel(int p){
   if(p < 1 || p > 100)return;
   playlevel = p;
   mvmost = 3;  /* ladder branch factor */
   if(playlevel > 50)mvmost = 4;
   if(playlevel > 200)mvmost = 5;
   
   noladder = FALSE;
   taclibs = 4;
   if(playlevel < 20)taclibs = 3;
   if(playlevel < 5)taclibs = 2;

   if(playlevel < 15)quicklevel = playlevel;
   else quicklevel = 15 + (playlevel - 15)/4;
   quicklibs = 3;
   if(playlevel < 20)quicklibs = 2;
   if(playlevel <= 1)noladder = TRUE;
   }

/* major default parameters */

void initinit(int l,int s){
	progname = "g2";
	cplay[0] = cplay[1] = TRUE;
	handicap = 0;
	graph = hp150 = FALSE;
	boardsize = s;
   	boardsquare = boardsize * boardsize;
   	firstsquare = 0;
   	lastsquare = boardsquare;
   	fixplaylevel(l);
   	timeflag = FALSE;
   	chineseflag = FALSE;
	highlightlast = FALSE;
	tournflag = FALSE;
	debugflag = FALSE;
	}



void turnoffcplay(void){
	cplay[0] = cplay[1] = FALSE;
	}

void outerror(int line,char* str){
	printf("%s\n",str);
	}

void psqr(int s){
	char tmp[10];
	ssqr(s,tmp);
	if(tournflag || xtflag)
		printf("%s",tmp);
	else
		printw("%s",tmp);
	}

void ssqr(int s,char* str){
	char c;
	if(s == PASS){
		strcpy(str,"pass");
		return;
		}
	if(s == NOSQUARE){
		strcpy(str,"NO  ");
		return;
		}
	if(s < 0 || s > (boardsize*boardsize-1))printw("ssqr, s is %d! ",s);
	c = s%boardsize + 'A';
	if(c > 'H')++c;
	snprintf(str,10,"%c%d ",c,boardsize-s/boardsize);
	return;
	}

