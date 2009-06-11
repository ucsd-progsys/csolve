/* Copyright 1993 David Fotland.
 * Permission granted to Spec to use and distribute as a computer benchamrk
 * permission is not granted to use or modify this code for any
 * purpose other than benchmarking or computer system performance tuning.
 *
 * This program is available (with a user interface) for:
 * IBM-PC/DOS from Ishi Press Inc San Jose CA (408)944-9900
 * Penpoint from PenGames San Jose Ca (408)985-1236
 */

# include "g2def.h"

extern int Xflag;	/* TRUE if output to X windows */
extern char *progname;
extern int highlightlast;
extern struct rulestruct rules[NUM_RULES];
extern int maxmove,mvmost;
extern int chineseflag;
extern int sc,scr;
extern int rmslowflag;
extern int beepflag;
extern int board[NUMSQUARES+1];
extern int eyerec[NUMSQUARES];
extern int eyevitrec[NUMSQUARES];
extern int eyefreelist;
extern int eyetype[NUMEYERECS], eyeval[NUMEYERECS], eyepot[NUMEYERECS], eyemin[NUMEYERECS];
extern int eyeptr[NUMEYERECS],eyevital[NUMEYERECS];
extern int shapebrd[NUMSQUARES];
extern int kosquare;
extern int numnodes,nummoves;
extern int gxstart,gxincr,gystart,gyincr;
extern int xmax,xmin,ymax,ymin;
extern int lookaheadflag;
extern int lookldr;
extern int edge[NUMSQUARES+1],fdir[NUMSQUARES],ldir[52],ldiag[52];
extern int edge2[NUMSQUARES+1];
extern int xval[NUMSQUARES],yval[NUMSQUARES];
extern int lnbf[NUMSQUARES][2];
extern int lnbn[NUMSQUARES];
extern int nblbp[NUMSQUARES];
extern int cnfreelist;            /* free connection records */
extern struct potential pots[NUMPOTENTIAL];
extern int armyfreelist;          /* free army records */
extern int armypot[NUMARMIES];
extern int armygroups[NUMARMIES];   /* list of groups in army */
extern int armydeadgroups[NUMARMIES];
extern int armyeyes[NUMARMIES];
extern int armylbp[NUMARMIES];
extern int armynbp[NUMARMIES];
extern int armyeyerecs[NUMARMIES];
extern int armyvitalpoints[NUMARMIES];
extern int armyterr[NUMARMIES];
extern int armyrun[NUMARMIES][NUMRUN];
extern int armyrn_pot[NUMARMIES];
extern int armywk_pot[NUMARMIES];
extern int armyeyespace[NUMARMIES];
extern int armyeyepotential[NUMARMIES];
extern int grdeadarmy[NUMGROUPS];
extern int grarmy[NUMGROUPS];     /* army number for group */
extern int armysize[NUMARMIES];   /* number of stones in army */
extern int armyalive[NUMARMIES];  /* aliveness of army */
extern int charmy;                /* armies which changed groups */
extern int chalive;               /* armies whose aliveness changed */
extern int armylibs[NUMARMIES];   /* liberty list for army */
extern int eyelist;
extern int strattotal[NUMSQUARES];
extern int stratguess[NUMSQUARES];
extern int stratgoodreasons[NUMSQUARES];
extern int stratreasons[NUMSQUARES];
extern int urgent[NUMSQUARES];
extern int nextstrat;
extern struct strategy strat[NUMSTRATS];
extern int scoreval[NUMSQUARES];
extern int handicap;
extern int boardsize;
extern int boardsquare,firstsquare,lastsquare;
extern long timeused[2];
extern int hcapbonus[];
extern int eyesdiags[NUMSQUARES];
extern int terhd;
extern int chgrp,terv[NUMSQUARES];
extern int rterv[NUMSQUARES][2];
extern int tscr,bstscr,rtscr,thrscr;
extern int list[NUMLIST];
extern int links[NUMLIST];
extern int freelist;
extern int pcls[NUMPCLS],pclsnext;
extern int cnbrd[NUMSQUARES],cnchgd;
extern int lkbrd[NUMSQUARES];
extern int llbrd[NUMSQUARES];
extern int sqrbrd[NUMSQUARES][4];
extern int dstbrd[NUMSQUARES][4];
extern int grlbp[NUMGROUPS];
extern int grldr[NUMGROUPS+NUMCONNS+NUMSQUARES];
extern int grcnp[NUMGROUPS];
extern int grnbp[NUMGROUPS];
extern int ldrflag[NUMSQUARES];
extern int sply[NUMPLY],eply[NUMPLY],scrply[NUMPLY];
extern int nbply[NUMMOVES],lbply[NUMMOVES]; 
extern int cplay[2],cplayed[2];
extern int graph,hp150; 
  
extern int tm;   /* side to move */ 
extern int stscr; 
extern int komi;
extern int grcolor[NUMGROUPS];
extern int grpieces[NUMGROUPS];
extern int level[];  /* level of play for computer */ 
extern int grlibs[NUMGROUPS]; 
extern int gralive[NUMGROUPS];
extern int grthreatened[NUMGROUPS];
extern int groldalive[NUMGROUPS];
extern int groldthreatened[NUMGROUPS];
extern int grlv[NUMGROUPS];
extern int grsize[NUMGROUPS]; 
extern int mvs[NUMMOVES];
extern int mvcapt[NUMMOVES],mvconn[NUMMOVES];
extern int mvcolor[NUMMOVES];
extern int mvnext[NUMMOVES],kosave[NUMMOVES];
extern int atariflag; 
extern int ld[NUMSQUARES];
extern int lgr[NUMSQUARES];
extern int ltr1[NUMSQUARES];
extern int ltr2[NUMSQUARES];
extern int ltrgd[NUMSQUARES]; 
extern int ltrscr;
extern int cngr1[NUMCONNS],cngr2[NUMCONNS];
extern int cncnum[NUMCONNS],cnlknum[NUMCONNS],cnllnum[NUMCONNS];
extern int cnptr[NUMCONNS],cnlkptr[NUMCONNS],cnllptr[NUMCONNS];
extern int cnprot[NUMCONNS],cntype[NUMCONNS];
extern int msptr;
extern int numpris[2];
extern int maxgr,maxpc;
extern int pscr,ctscr;
extern int evcolor;
  
extern int nbr[52],diags[52];
extern int *clr[];
  
extern int debug,playlevel,quicklevel,taclibs,quicklibs;
extern int problemflag;
extern int ahead,phase,urgdefarmies;
  
extern int jflag[4],jreflect[4];
extern int jptr2[4],jcolor[4];
extern int tournflag;

 
extern int termtype;
extern int takestones;

extern int cfac[2];
extern int sumpots[52];
extern int ltrfac[NUM_LIVENESS_STATES],pfac[NUM_LIVENESS_STATES],defv[NUM_LIVENESS_STATES],atkv[NUM_LIVENESS_STATES],kval[NUM_LIVENESS_STATES],ctval[NUM_LIVENESS_STATES],connval[NUM_LIVENESS_STATES],
  sumeyes[52],
  thalive[NUM_LIVENESS_STATES];
extern int dirnm[52],opdir[4];

extern int linktypes[52],brddir[4];
extern int nbri[52],diagsi[52];
