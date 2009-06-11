/* Copyright 1993 David Fotland.
 * Permission granted to Spec to use and distribute as a computer benchamrk
 * permission is not granted to use or modify this code for any
 * purpose other than benchmarking or computer system performance tuning.
 *
 * This program is available (with a user interface) for:
 * IBM-PC/DOS from Ishi Press Inc San Jose CA (408)944-9900
 * Penpoint from PenGames San Jose Ca (408)985-1236
 */



# include "g2hd.h"

  
char * NTS ident = "@(#) Rev 6.03 Copyright (c) 1988, 1989, 1993 David Fotland.  All rights reserved.";


extern int linktypes[],brddir[];
extern int diffs4[5][3],diffs5[9][4],diffs6[4][5],nbri[],diagsi[];

/* NOTE: originally, it was diffs4i[5][3], Raymond changed it to [7][3] */
extern int diffs4i[7][3],diffs5i[9][4],diffs6i[4][5];

# define NUMHC 65
int hcpoints;  
  
 
void initkomi(void){
	komi = 0;
	if(handicap != 0)return;
	if(chineseflag){
		if(boardsize == 19)komi = 50*8;
		else komi = 50*4;
		}
	else if(boardsize == 19)komi = 50*5+25;
	}
	       
int lastrandom;

/* initailize all constants that change when board size changes */

void initbsizeconst(void){
   int i,j,x,y,ixmin,iymin,xm,ym,mid;
   firstsquare = 0;
   boardsquare = boardsize*boardsize;
   lastsquare = boardsquare;
   lastrandom = boardsize/3;
   brddir[0] = -boardsize;
   brddir[3] = boardsize;
   initkomi();
   for(i = 0; i < 52; ++i)linktypes[i] = 0;
   linktypes[1] = linktypes[boardsize] = 1;
   linktypes[boardsize-2] = linktypes[boardsize+2] =
      linktypes[boardsize*2+1] = linktypes[boardsize*2-1] = 2;
   for(i = 0; i < 5; ++i)
      for(j = 0; j < 3; ++j)
         if(diffs4i[i][j] > 10)diffs4[i][j] = diffs4i[i][j] - 19 + boardsize;
         else diffs4[i][j] = diffs4i[i][j];
   for(i = 0; i < 9; ++i)
      for(j = 0; j < 4; ++j)
         if(diffs5i[i][j] > 10)diffs5[i][j] = diffs5i[i][j] - 19 + boardsize;
         else diffs5[i][j] = diffs5i[i][j];
   for(i = 0; i < 4; ++i)
      for(j = 0; j < 5; ++j)
         if(diffs6i[i][j] > 10)diffs6[i][j] = diffs6i[i][j] - 19 + boardsize;
         else diffs6[i][j] = diffs6i[i][j];
  
  
   for(i = 0; i < 52; ++i){
      if(nbri[i] == 19)nbr[i] = boardsize;
      else if(nbri[i] == -19)nbr[i] = -boardsize;
      else nbr[i] = nbri[i];
      if(diagsi[i] == -18)diags[i] = -boardsize+1;
      else if(diagsi[i] == -20)diags[i] = -boardsize-1;
      else if(diagsi[i] == 18)diags[i] = boardsize-1;
      else if(diagsi[i] == 20)diags[i] = boardsize+1;
      }
 
   edge[NOSQUARE] = edge2[NOSQUARE] = NOEDGE;
   for(i = 0; i < boardsquare; i++){
      x = xval[i] = i%boardsize;
      y = yval[i] = i/boardsize;
      xm = x+1;
      if(boardsize - x < xm)xm = boardsize-x;
      ym = y+1;
      if(boardsize - y < ym)ym = boardsize-y;
      if(xm < ym){
         edge[i] = xm;
         edge2[i] = ym;
         }
      else {
         edge[i] = ym;
         edge2[i] = xm;
         }
      if(x == 0){
         fdir[i] = 11;
         }
      if(x == boardsize-1){
         fdir[i] = 14;
         }
      if(y == 0){
         fdir[i] = 8;
         }
      if(y == boardsize-1){
         fdir[i] = 17;
         }
      if(edge[i] != 1){     /* fdir and ldir for center */
         ixmin = x;
         iymin = y;
         mid = boardsize/2;
         if(boardsize-1-x < ixmin)ixmin = boardsize-1-x;
         if(boardsize-1-y < iymin)iymin = boardsize-1-y;
         if(ixmin == iymin){    /* diagonal */
            if(x <= mid && y <= mid){
               fdir[i] = 20;
               }
            if(x > mid && y <= mid){
               fdir[i] = 24;
               }
            if(x <=mid && y > mid){
               fdir[i] = 28;
               }
            if(x > mid && y > mid){
               fdir[i] = 32;
               }
            }
         else if(iymin < ixmin){ /* top or bottom */
            if(y < mid){
               fdir[i] = 48;
               }
            else {
               fdir[i] = 36;
               }
            }
         else {
            if(x < mid){
               fdir[i] = 44;
               }
            else {
               fdir[i] = 40;
               }
            }
         }
      }
   edge[0] = 0;
   edge[boardsquare-1] = 0;
   edge[boardsize-1] = 0;
   edge[boardsquare-boardsize] = 0;
   edge2[0] = 0;
   edge2[boardsquare-1] = 0;
   edge2[boardsize-1] = 0;
   edge2[boardsquare-boardsize] = 0;
   fdir[0] = 0;
   fdir[boardsquare-1] = 6;
   fdir[boardsize-1] = 2;
   fdir[boardsquare-boardsize] = 4;
   initrtval();
   initshapes();
   }



  
void initjflags(void){
   int i;
   for(i = 0; i < 4; ++i){
      jflag[i] = 1;
      jptr2[i] = 0;
      jreflect[i] = 0;
      }
   }


void initvars(void){	/* initialize variables */
   int i,j,s,ldtmp;
   maxmove = NUMMOVES - NUMPLY;
   msptr = 0;
   numnodes = nummoves = 0;
   takestones = FALSE;
   nextstrat = 0;
   scr = 0;
   tscr = 0;
   pscr = 0;
   atariflag = FALSE;
   ltrscr = 0;
   urgdefarmies = EOL;
   xmax = ymax = 0;
   xmin = ymin = boardsize-1;
   chgrp = EOL;
   for(i = 0; i < NUMLIST-1; i++)links[i] = i+1;
   freelist = 0;
   links[NUMLIST-2] = EOL;
   list[NUMLIST-1] = BIGNUM;
   eyefreelist = EOL;
   for(i = NUMEYERECS-1; i > 0; --i)adflist(i,&eyefreelist);
	/* keep eye zero out of list */
   for(i = 0; i < NUMEYERECS; i++){
	eyetype[i] = NOEYE;
	eyeptr[i] = EOL;
	eyevital[i] = EOL;
	eyeval[i] = 0;
	eyepot[i] = 0;
	eyemin[i] = 0;
	}
   for(s = 0; s < boardsquare; ++s){
      stratreasons[s] = EOL;
      strattotal[s] = 0;
      stratgoodreasons[s] = FALSE;
      stratguess[s] = 0;
      eyerec[s] = 0;
      eyevitrec[s] = EOL;
      }

   initjflags();
   for(i = 0; i < NUMMOVES; ++i){
      lbply[i] = nbply[i] = EOL;
      }
   cnfreelist = EOL;
   for(i = NUMCONNS-1; i >= 0; --i)
      adflist(i,&cnfreelist);
   pots[0].pot_val = 0;
   pots[0].pot_type = NOPOT;
   armyfreelist = EOL;
   for(i = NUMARMIES-2; i >= 0; --i)
      adflist(i,&armyfreelist);
   for(i = NUMARMIES-1; i >= 0; --i){
      armyvitalpoints[i] = EOL;
      armyalive[i] = 1;
      armygroups[i] = EOL;
      armydeadgroups[i] = EOL;
      armypot[i] = EOL;
      armysize[i] = 0;
      armylibs[i] = 0;
      armylbp[i] = EOL;
      armynbp[i] = EOL;
      armyeyerecs[i] = EOL;
      armyeyes[i] = 0;
      armyeyepotential[i] = 0;
      armyeyespace[i] = 0;
      armyterr[i] = 0;
	for(j = 0; j < NUMRUN; ++j)armyrun[i][j] = EOL;
      armyrn_pot[i] = 0;
      armywk_pot[i] = 0;
      }
   charmy = EOL;
   chalive = EOL;
   lookldr = EOL;
   eyelist = EOL;
   for(i = 0; i < NUMMOVES; ++i){
      mvs[i] = 0;
      mvconn[i] = mvcapt[i] = EOL;
      mvcolor[i] = 0;
      mvnext[i] = -1;
      kosave[i] = NOSQUARE;
      }
   pclsnext = 0;
   tm = 0;
   evcolor = 2;
   terhd = EOL;
   kosquare = NOSQUARE;
   for(i = firstsquare; i < lastsquare; i++){
      lnbf[i][0] = lnbf[i][1] = 0;
      lnbn[i] = 4;
      nblbp[i] = EOL;
      ld[i] = NOLD;
      j = fdir[i];
      for(ldtmp = ldir[j]; j < ldtmp; ++j)addlist(i+nbr[j],&nblbp[i]);
      if(xval[i] == 0 || xval[i] == boardsize-1 ||
        yval[i] == 0 || yval[i] == boardsize-1)
        lnbn[i] = 3;
      board[i] = NOGROUP;
      shapebrd[i] = EOL;
      ld[i] = NOLD;
      eyesdiags[i] = EOL;
      ldrflag[i] = EOL;
      lgr[i] = NOGROUP;
      terv[i] = 0;
      cnbrd[i] = EOL;
      lkbrd[i] = EOL;
      llbrd[i] = EOL;
      ltrgd[i] = FALSE;
      ltr1[i] = 0;
      ltr2[i] = 0;
      for(j = 0; j < 4; ++j){
         sqrbrd[i][j] = NOSQUARE;
         dstbrd[i][j] = 0;
         }
      }
   for(i = 0; i < NUMGROUPS+NUMCONNS+NUMSQUARES; ++i)
      grldr[i] = EOL;
   board[NOSQUARE] = NOGROUP;
   cnchgd = EOL;
   lnbn[0] = lnbn[boardsquare-1] = lnbn[boardsize-1] =
       lnbn[boardsquare-boardsize] = 2;
   lookaheadflag = FALSE;
   for(i = 0; i < NUMCONNS; i++){
      cngr1[i] = NOGROUP;
      cngr2[i] = NOGROUP;
      cncnum[i] = 0;
      cnlknum[i] = 0;
      cnllnum[i] = 0;
      cnptr[i] = EOL;
      cnlkptr[i] = EOL;
      cnllptr[i] = EOL;
      cnprot[i] = CANT_CONNECT;
      cntype[i] = 0;
      }
   for(i = 0; i < NUMGROUPS; i++){
      grcolor[i] = NOCOLOR;
      grlbp[i] = EOL;
      grnbp[i] = EOL;
      grcnp[i] = EOL;
      grlibs[i] = 0;
      gralive[i] = 0;
      grthreatened[i] = FALSE;
      groldalive[i] = 0;
      groldthreatened[i] = FALSE;
      grlv[i] = FALSE;
      grsize[i] = 0;
      grpieces[i] = -1;
      grarmy[i] = NOARMY;
      grdeadarmy[i] = NOARMY;
      }
   grcolor[NOGROUP] = NOCOLOR;
   grpieces[NOGROUP] = -1;
   grarmy[NOGROUP] = -1;
   maxgr = maxpc = 0;
   numpris[0] = numpris[1] = 0;
   timeused[0] = timeused[1] = 0;
   hcpoints = EOL;
   if(boardsize >= 13){
          addlist(3*boardsize+3,&hcpoints);
          addlist(4*boardsize-4,&hcpoints);
          addlist(boardsquare-3*boardsize-4,&hcpoints);
          addlist(boardsquare-4*boardsize+3,&hcpoints);
          }
   else {
          addlist(3*boardsize-3,&hcpoints);
          addlist(boardsquare-3*boardsize+2,&hcpoints);
          addlist(2*boardsize+2,&hcpoints);
          addlist(boardsquare-2*boardsize-3,&hcpoints);
          }
   if(boardsize >= 13 && boardsize%2 == 1){
      addlist(boardsquare/2,&hcpoints);
      addlist(3*boardsize+boardsize/2,&hcpoints);
      addlist(boardsquare/2-boardsize/2+3,&hcpoints);
      addlist(boardsquare/2+boardsize/2-3,&hcpoints);
      addlist(boardsquare-3*boardsize-boardsize/2-1,&hcpoints);
      } 
   }
  
  
  
