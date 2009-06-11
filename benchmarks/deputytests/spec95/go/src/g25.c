/* Copyright 1993 David Fotland.
 * Permission granted to Spec to use and distribute as a computer benchamrk
 * permission is not granted to use or modify this code for any
 * purpose other than benchmarking or computer system performance tuning.
 *
 * This program is available (with a user interface) for:
 * IBM-PC/DOS from Ishi Press Inc San Jose CA (408)944-9900
 * Penpoint from PenGames San Jose Ca (408)985-1236
 */

# include <math.h>
# include <stdlib.h>
# include "g2hd.h"
  
extern int noladder,brddir[4];
  
int splitlist;	/* list of groups whose armies need splitting */  
  
int nextpot;  /* next free potential record */
  
/* is group with this aliveness threatened with capture.  Used
   for sente and for adjusting score */

int thalive[NUM_LIVENESS_STATES] = { 0,0,
0,0,0,0,0,
0,
0,0,0,
0,0,1,1,
0,0,1,1,
0,0,0,0,
0,0,0 };

/* values for pieces on board by aliveness */
  
int pfac[NUM_LIVENESS_STATES] = {0,0,
50,50,50,50,45,	/* has two eyes or space for two eyes */
48,             /* seki */
45,45,   	/* has miai for two eyes */
40,             /* wins semeai */
0,35,0,0,    /* has eye potential for two eyes; unsettled */
35,25,25,0,-10,       /* has some eye potential */
-40,-40,-48, /* probably dead */
0,		/* probably dead (SMOTHERED) */
-50,		/* tactically threatened */
-50 };		/* captured (DEAD) */
 

/* values for open points by aliveness */

int ltrfac[NUM_LIVENESS_STATES] = { 0,0,
50,50,50,50,45,		/* has two eyes or space for two eyes */
48,45,45,		/* has miai for two eyes */
40,                     /* wins semeai */
0,35,0,0, 		/* has eye potential for two eyes (unsettled) */
35,25,25,0,-10,     	/* has some eye potential */
-40,-40,-48, 	/* probably dead */
0, 			/* probably dead (SMOTHERED)*/
-50,			/* tactically threatened */
-50 };			/* captured */
  

/* values for defending group for each stone or liberty */

int defv[NUM_LIVENESS_STATES] = {
0,0,
0,0,0,0,10,	/* live groups */
0,0,10,10,	/* groups which can get 2 eyes */
20,10,40,50,	/* unsettled groups */
10,20,35,35,20,	/* groups with some eyespace */
5,5,5,	/* dead groups */
0,2,0 };


/* values for attacking group for each stone or liberty */

int atkv[NUM_LIVENESS_STATES] = {
0,0,
0,0,0,0,5,	/* live groups */
0,0,5,5,	/* groups which can get 2 eyes */
20,10,40,50,	/* unsettled groups */
20,20,30,40,20,	/* groups with some eyespace */
5,5,5,	/* dead groups */
0,2,0 };

/* value of cutting off a group in addition to the attacking value */

int ctval[NUM_LIVENESS_STATES] = { 0,0,
0,0,0,0,5,  /* living groups */
5,5,10,10,  /* living groups */
15,15,30,20,  /* unsettled groups */
30,30,40,40,30,  /* has some eye potential */
50,50,50,  /* dead groups */
50,25,0 };

/* value of connecting a group */

int connval[NUM_LIVENESS_STATES] = { 0,0,
0,0,0,0,4,  /* living groups */
4,5,10,10,  /* living groups */
20,10,20,20,  /* unsettled groups */
40,50,50,75,50,  /* has some eye potential */
100,100,100,  /* dead groups */
100,50,0 };



/* bonus for changing group from alive or unsettled to weak or dead */

int kval[NUM_LIVENESS_STATES] = {
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  /* never see */
10,40,50,50,50,
75,75,75,
0,
90,
95 };


/* sumeyes is numer of eyes for territory ( 8 is one eye ) */
# define MAXTERR 18


//matth: Not sure why these two arrays are so long.  The comments seem
//to indicate that only 0 thru MAXTERR(inclusive) are referenced.
int sumeyes[52] = { 
0,4,4,8,8,8,12,12,16,16,20,20,20,24,24,24,25,26,26, /* up to MAXTERR */
26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26 };

int sumpots[52] = {
0,0,0,0,4,8,4,4,4,4,4,4,4,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };


/* territory for extend down open side from 1 stone wall
 * by edge value
 */

int terr1[] =  /* should match right column of terr5 */
{ 0,0,3,6,7 };

/* territory for crawling along edge under enemy stone
 * by edge value
 */

int terr2[] = 
{ 0,1,1,3,5 };

/* territory for extending down edge from a two stone wall
 * by edge value of stone nearest edge
 */

int terr3[] =
{ 0,1,6,7,8 };

/* territory for extending down edge when have to go down
 * a line to get under an enemy stone
 */

int terr4[] = 
{ 0,0,1,4,6 };

/* terr5 is the territory for extending down side on a line toward a
 * enemy stone at a distance. Enemy stone is on same line as stone
 * extending from
 */

int terr5[5][6] =
{
	{ 0,0,0,0,0,0 },	/* edge = 0 */
	{ 0,0,0,0,0,0 },	/* edge = 1 */
	{ 0,1,2,3,3,3 },	/* edge = 2 */
	{ 0,1,3,5,6,6 },	/* edge = 3 */
	{ 0,2,5,6,7,7 },	/* edge = 4 */
};

extern int cfac[];

void markspot(int s){
      int ptr,bptr;
      if(cnbrd[s] != EOL)
         mrglist(cnbrd[s],&cnchgd);
      if(lkbrd[s] != EOL)
         mrglist(lkbrd[s],&cnchgd);
      if(llbrd[s] != EOL)
         mrglist(llbrd[s],&cnchgd);
      mrglist(ldrflag[s],&lookldr);
      if(board[s] != NOGROUP){
	      gralive[board[s]] |= 32;
	      mrglist(grcnp[board[s]],&cnchgd);

	      /* in case move added stone to group */
	      for(bptr = grpieces[board[s]]; bptr != -1; bptr = mvnext[bptr])
		      for(ptr = ldrflag[mvs[bptr]]; ptr != EOL; ptr = links[ptr]){
			      if(list[ptr] >= NUMGROUPS+NUMCONNS)
				      mrglist(eyeptr[eyerec[list[ptr]-NUMGROUPS-NUMCONNS]],&eyelist);
			      else if(list[ptr] >= NUMGROUPS)
				      addlist(list[ptr]-NUMGROUPS,&cnchgd);
			      else gralive[list[ptr]] |= 32;
			      }
	      }
      else {
	      for(ptr = ldrflag[s]; ptr != EOL; ptr = links[ptr]){
		      if(list[ptr] >= NUMGROUPS+NUMCONNS)
			      mrglist(eyeptr[eyerec[list[ptr]-NUMGROUPS-NUMCONNS]],&eyelist);
		      else if(list[ptr] >= NUMGROUPS)
			      addlist(list[ptr]-NUMGROUPS,&cnchgd);
		      else gralive[list[ptr]] |= 32;
		      }
	      }
      }

/* look at places where pieces were added or removed from board since last
 * evaluation.  mark neighboring groups, connections
 * for reevaluation.  Eyes are marked in g2eye.c in findeyelist
 */
  

void markpcls(void){	/* mark groups to reeval before ladder */
   int lptr,s,i,ldtmp,j,ldtm2,k,ldtm3,sn,sn2,sn3,ptr,cn;
   for(ptr = lookldr; ptr != EOL; ptr = links[ptr]){
         if(list[ptr] >= NUMGROUPS+NUMCONNS)
            mrglist(eyeptr[eyerec[list[ptr]-NUMGROUPS-NUMCONNS]],&eyelist);
         else if(list[ptr] >= NUMGROUPS){
	    cn = list[ptr]-NUMGROUPS;
	    if(cncnum[cn] != 0 || cnlknum[cn] != 0 || cnllnum[cn] != 0)
		    addlist(cn,&cnchgd);
	    }
         else if(gralive[list[ptr]] != DEAD)
	    gralive[list[ptr]] |= 32;
         }
   killist(&lookldr);

   for(lptr = 0; lptr < pclsnext; ++lptr){

      s = pcls[lptr];

      markspot(s);

      i = fdir[s];
      for(ldtmp = ldir[i]; i < ldtmp; ++i){
	      sn = s + nbr[i];
	      markspot(sn);
	      if(board[sn] == NOGROUP){
		      j = fdir[sn];
		      for(ldtm2 = ldir[j]; j < ldtm2; ++j){
			      sn2 = sn + nbr[j];
			      markspot(sn2);

			      if(board[sn] == NOGROUP){
				      k = fdir[sn2];
				      for(ldtm3 = ldir[k]; k < ldtm3; ++k){
					      sn3 = sn2 + nbr[k];
					      markspot(sn3);
					      }
				      }      
			      }
		      }
	      }


      if(board[s] == NOGROUP)
	      ptr = ldrflag[s];
      else
	      ptr = ldrflag[mvs[grpieces[board[s]]]];
      for(; ptr != EOL; ptr = links[ptr]){
         if(list[ptr] >= NUMGROUPS+NUMCONNS)
            mrglist(eyeptr[eyerec[list[ptr]-NUMGROUPS-NUMCONNS]],&eyelist);
         else if(list[ptr] >= NUMGROUPS)
            addlist(list[ptr]-NUMGROUPS,&cnchgd);
         else gralive[list[ptr]] |= 32;
         }
      }
   }

/* get rid of the ladder flags for ladder g (group number, connection, or
 * eye.
 */

void kill_ldrflags(int g){
	int ptr;
	for(ptr = grldr[g]; ptr != EOL; ptr = links[ptr])
		dellist(g,&ldrflag[list[ptr]]);
	killist(&grldr[g]);
	}


/* return TRUE if group g can't be captured if it moves first.
 * can't be captured if it gets mlibs+1 in one move, or if it
 * can capture a neighboring group big enough
 */

int cantbecaptured(int g,int mlibs){
	int libs,maxlibs,tmplist= EOL,ptr,ptr2,cn,g2;
	int libstoescape;
	libstoescape = mlibs+1;
	maxlibs = grlibs[g];
	/* look for neighboring groups which can be captured */

	if(maxlibs < libstoescape){
	    tmplist = EOL;
	    for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
		g2 = list[ptr];
		if(grlibs[g2] == 1){
			if(grlibs[g] == 1 && list[grlbp[g]] == list[grlbp[g2]])
				continue;  /* snapback */
			libs = grlibs[g];
			cpylist(grlbp[g],&tmplist);
			libs += grsize[g2];
			if(mrglist(grlbp[g2],&tmplist) == 0)--libs;
			for(ptr2 = grnbp[g2]; ptr2 != EOL; ptr2 = links[ptr2])
				if(list[ptr2] != g)
					libs += mrglist(grlbp[list[ptr2]],&tmplist);
			killist(&tmplist);
			if(libs > maxlibs)maxlibs = libs;
			if(maxlibs >= libstoescape)break;
			}
		}
	    }

	/* look for extensions and connections for new libs */

	if(maxlibs < libstoescape)
	    for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		libs = grlibs[g];
		cpylist(grlbp[g],&tmplist);
		libs += mrglist(nblbp[list[ptr]],&tmplist)-1;
		for(ptr2 = cnbrd[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
			cn = list[ptr2];
			if(cngr1[cn] != g && cngr2[cn] != g)continue;
			if(cngr1[cn] == g)
				g2 = cngr2[cn];
			else
				g2 = cngr1[cn];
			if(cncnum[cn] != 0 || grlibs[g] > 2)
				libs += mrglist(grlbp[g2],&tmplist);
			}
		if(libs > maxlibs)maxlibs = libs;
		killist(&tmplist);
		if(maxlibs >= libstoescape)break;
		}
	if(maxlibs >= libstoescape){
		return(TRUE);
		}
	return(FALSE);
	}


/* canbethreatened returns true if a group can possibly be captured if
 * the opponent moves first
 */


int canbethreatened(int g){
	int maxlibs;
	if(grlibs[g] > taclibs){
		return(FALSE);
		}
	maxlibs = getefflibs(g,taclibs,g);
	if(maxlibs > taclibs){
		return(FALSE);
		}
	return(TRUE);
	}

/* return TRUE if g is a cutting stone (conservatively).  armies aren't figured
 * out yet so just look at nearby groups
 */

int cutstone(int g){
	if(grnbp[g] == EOL || links[grnbp[g]] == EOL)return(FALSE);
	return(TRUE);
	}

/* findcaptured finds all the groups which are tactically simple to
 * capture even if group color moves first.  These contribute to
 * eyes and unbreakable connections.
 * it always evaluates every group.  It is not incremental.
 */


void findcaptured(void){
	int g,g2,comblist=EOL,ptr,ptr2,lvl,libs;
/*	if(noladder)return; */
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		lvl = playlevel;
		libs = taclibs;
		if(!cutstone(g)){
			lvl = quicklevel; 
			libs = quicklibs;
			}
		if(grlibs[g] > taclibs || cantbecaptured(g,taclibs)){
			if(gralive[g] == DEAD)gralive[g] |= 32;
			}
		else if(iscaptured(g,80,lvl,libs,grcolor[g],NOGROUP)){
			newdeadgroup(g,DEAD,gralive[g]&31);
			addlist(g,&comblist);
			}
		else if(gralive[g] == DEAD){
			gralive[g] |= 32;
			}
		if(gralive[g] == (32 | DEAD))
      			fixwasdeadgroup(g);
		}
	for(ptr = comblist; ptr != EOL; ptr = links[ptr]){
		g = list[ptr];
      		for(ptr2 = grnbp[g]; ptr2 != EOL; ptr2 = links[ptr2])
	 		if(gralive[list[ptr2]] != DEAD && 
			   !grthreatened[list[ptr2]])break; 
      		g2 = list[ptr2];
      		if(ptr2 != EOL){
         		for(ptr2 = links[ptr2]; ptr2 != EOL; ptr2 = links[ptr2]){
            			if(gralive[list[ptr2]] != DEAD &&
				   !grthreatened[list[ptr2]]){
               				combinearmy(grarmy[list[ptr2]],grarmy[g2]);
					}
            			}

         		addlist(g,&armydeadgroups[grarmy[g2]]);
         		grdeadarmy[g] = grarmy[g2];
         		}
      		}
   	killist(&comblist);
	}



/* bdead finds groups which are easily captured so they can count
 * as connections and eyes
 */

void bdead(void){ 
   int g,flag;
   findcaptured();	/* find DEAD groups */
   flag = evcolor != tm;
  
   for(g = 0; g < maxgr; ++g){
      if(!grlv[g])continue;
      if(gralive[g] == DEAD && grthreatened[g]){
	grthreatened[g] = FALSE;
	fixwasthgroup(g);
	}
      if(gralive[g] == DEAD)continue;
      if(grlibs[g] == 1 && list[grlbp[g]] != kosquare && gralive[g] != DEAD){
	grthreatened[g] = TRUE;
	newdeadgroup(g,SMOTHERED,gralive[g]&31);
        gralive[g] |= 32;
	}
      else if(flag || gralive[g] >= 32){   /* found ladder cantidate */

         if(/* !noladder && */
	    (grlibs[g] > 1 || list[grlbp[g]] != kosquare) &&
            gralive[g] != DEAD){
		 if(grldr[g] != EOL)kill_ldrflags(g);
		 if(canbethreatened(g) && 
		    iscaptured(g,80,playlevel,taclibs,1-grcolor[g],g)){
			 grthreatened[g] = TRUE;
			 newdeadgroup(g,SMOTHERED,gralive[g]&31);
			 gralive[g] |= 32;
			 }
		 else if(grthreatened[g]){
			 grthreatened[g] = FALSE;
			 fixwasthgroup(g);
			 }
		 }
         else if(grthreatened[g]){
		 grthreatened[g] = FALSE;
		 fixwasthgroup(g);
		 }
         }  /* end of if !noladder  */

      }  /* end of groups loop */
   }
  
  
void newdeadgroup(int g,int newaliv,int oldalive){
   int ptr,ptr2,s;

   armyeyes[grarmy[g]] = 0;
   killist(&armyeyerecs[grarmy[g]]);
   killist(&armyvitalpoints[grarmy[g]]);


   if(grdeadarmy[g] != NOARMY){ 
      addlist(list[armygroups[grdeadarmy[g]]],&splitlist);
      }

   if(armysize[grarmy[g]] > grsize[g]){
      addlist(g,&splitlist);
      }
   else {  /* wont be split, so delete dead armies here */
      	for(ptr = armydeadgroups[grarmy[g]]; ptr != EOL; ptr = links[ptr]){
		grdeadarmy[list[ptr]] = NOARMY;
		}
        killist(&armydeadgroups[grarmy[g]]);
	}

   pscr = pscr + (pfac[newaliv] - pfac[oldalive])*grsize[g]*
       cfac[grcolor[g]];
   adflist(g,&chgrp);
   markgroup(g);

   for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
      s = list[ptr];
      if(gralive[lgr[s]] != DEAD)
	      lgr[s] = g;
      }
   mrglist(grcnp[g],&cnchgd);  /* not redundant! */
   gralive[g] = newaliv;
   if(grthreatened[g])
	   for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
		   for(ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2])
			   if(cncnum[list[ptr2]] == 1)
				   addlist(list[ptr2],&cnchgd);
   }


void markgroup(int g){
	int ptr,ptr2,ptr3,s,sn,x,y,i,ldtmp;
   	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		if(cnbrd[list[ptr]] != EOL)
			mrglist(cnbrd[list[ptr]],&cnchgd);
		if(lkbrd[list[ptr]] != EOL)
			mrglist(lkbrd[list[ptr]],&cnchgd);
		if(llbrd[list[ptr]] != EOL)
			mrglist(llbrd[list[ptr]],&cnchgd);
		if(eyerec[list[ptr]] != 0)addlist(list[ptr],&eyelist);
		mrglist(eyesdiags[list[ptr]],&eyelist);
		for(ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
			if(cnbrd[list[ptr2]] != EOL)
				mrglist(cnbrd[list[ptr2]],&cnchgd);
			if(lkbrd[list[ptr2]] != EOL)
				mrglist(lkbrd[list[ptr2]],&cnchgd);
			if(llbrd[list[ptr2]] != EOL)
				mrglist(llbrd[list[ptr2]],&cnchgd);
			if(eyerec[list[ptr2]] != 0)addlist(list[ptr2],&eyelist);
			mrglist(eyesdiags[list[ptr2]],&eyelist);
			for(ptr3 = nblbp[list[ptr2]]; ptr3 != EOL; ptr3 = links[ptr3]){
				if(lkbrd[list[ptr3]] != EOL)
					mrglist(lkbrd[list[ptr3]],&cnchgd);
				if(llbrd[list[ptr3]] != EOL)
					mrglist(llbrd[list[ptr3]],&cnchgd);
				}
			}			
		}
   	for(s = grpieces[g]; s != -1; s = mvnext[s]){
   		sn = mvs[s];
		addlist(sn,&eyelist);
		if(eyesdiags[sn] != EOL)
			mrglist(eyesdiags[sn],&eyelist);
		i = fdir[sn];
		for(ldtmp = ldiag[i]; i < ldtmp; ++i)
			if(cnbrd[sn+diags[i]] != EOL)
				mrglist(cnbrd[sn+diags[i]],&cnchgd);
        x = xval[sn];
        y = yval[sn];
        if(x-4 < xmin)xmin = x-4;
        if(x+4 > xmax)xmax = x+4;
        if(y-4 < ymin)ymin = y-4;
        if(y+4 > ymax)ymax = y+4;
   		}
	}


/* group was threatened.  Must update its connections and territory
 * if nbr has THREAT connections, they must be reevaled.
 */


void fixwasthgroup(int g){
	int ptr,ptr2;

	markgroup(g);

	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
		for(ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2])
			if(cntype[list[ptr2]] == CN_THREAT)
				addlist(list[ptr2],&cnchgd);

	if(gralive[g] != DEAD)
   	    for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){

      		if(gralive[list[ptr]] == DEAD){
          		if(grdeadarmy[list[ptr]] != NOARMY){

         			combinearmy(grarmy[g],grdeadarmy[list[ptr]]);
				}
			else {
				grdeadarmy[list[ptr]] = grarmy[g];
				addlist(list[ptr],&armydeadgroups[grarmy[g]]);
				}
         		}

      		}
	}

/* group was dead and is no more.  must update its armies, connections,
 * eyes, and territory
 */

void fixwasdeadgroup(int g){
   int ptr;

   if(grdeadarmy[g] != NOARMY){
      addlist(list[armygroups[grdeadarmy[g]]],&splitlist);

      dellist(g,&armydeadgroups[grdeadarmy[g]]);
      grdeadarmy[g] = NOARMY;
      }
   markgroup(g);

   for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){

      if(gralive[list[ptr]] == DEAD){
          if(grdeadarmy[list[ptr]] != NOARMY){

             combinearmy(grarmy[g],grdeadarmy[list[ptr]]);
	     }
	  else {
		grdeadarmy[list[ptr]] = grarmy[g];
		addlist(list[ptr],&armydeadgroups[grarmy[g]]);
		}
          }

      }
   }


   


void fixarmies(void){
   int split1,ptr;
   split1 = EOL;        /* fix up armies */
   for(ptr = splitlist; ptr != EOL; ptr = links[ptr])
      addlist(grarmy[list[ptr]],&split1);
   killist(&splitlist);
   for(ptr = charmy; ptr != EOL; ptr = links[ptr]){

      if(board[list[ptr]] != NOGROUP)
         addlist(grarmy[board[list[ptr]]],&split1);
      }
   killist(&charmy);
   for(ptr = split1; ptr != EOL; ptr = links[ptr])
      splitarmy(list[ptr]);
   killist(&split1);
   }



/* figure out the aliveness of all groups on board.
 * as side effect, must figure out conections and
 * radiate thickness
 */


void life(void){
/*   fixonelibgroups(); */
   splitlist = EOL;
   markpcls();          /* mark eyes, connections, and groups for reeval */
   bdead();		/* find groups which are captured */
   fixli();		/* find eyes */
   fixcnprot();		/* find protected connections */

   fixarmies();         /* collect groups into armies */
   
   fixsmothered();	/* find armies which look dead */
   upltr();		/* find edge territory */
   fixgralive();	/* make all aliveness values correct */
   radiateweak();       /* radiate weak territory */
   }

/* must find alive 24s (SMOTHERED) before eyespace territory since they don't undercut 
 * a smothered group has one or two stones, no dead neighbors, one or two
 * liberties, no threatened neighbors, all liberties next to opponents
 * groups, and no way to get more liberies
 */

void fixsmothered(void){
	int g,ptr,x,y,smothered,ptr2;
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		if(gralive[g] == DEAD)continue;
		smothered = FALSE;
		if(grsize[g] < 3 && grcnp[g] == EOL && grnbp[g] != EOL &&
		   grlibs[g] < 3 && !isseki(grarmy[g]))smothered = TRUE;

		if(smothered)
			for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
				if(grthreatened[list[ptr]] || 
				   gralive[list[ptr]] == DEAD){
					smothered = FALSE;
					break;
					}
		if(smothered)
			for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
				if(ld[list[ptr]] != NEUTRALLD || lnbn[list[ptr]] > 2){
					smothered = FALSE;
					break;
					}
				for(ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2])
					if(ld[list[ptr2]] != NEUTRALLD || lnbn[list[ptr2]] > 2){
						smothered = FALSE;
						break;
						}
				}
		if(smothered){
			gralive[g] &= 31;
			if(gralive[g] != SMOTHERED){

				pscr = pscr + (pfac[SMOTHERED] - pfac[gralive[g]&31])*grsize[g]*
					cfac[grcolor[g]];
				addlist(g,&chgrp);
				gralive[g] = SMOTHERED;
				armyeyes[grarmy[g]] = 0;
				for(ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr]){
					x = xval[mvs[ptr]];
					y = yval[mvs[ptr]];
					if(x-4 < xmin)xmin = x-4;
					if(x+4 > xmax)xmax = x+4;
					if(y-4 < ymin)ymin = y-4;
					if(y+4 > ymax)ymax = y+4;
					}
				}
			}
		else if((gralive[g]&31) == SMOTHERED){ /* was SMOTHERED, is no more */
			gralive[g] |= 32;
			for(ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr]){
				x = xval[mvs[ptr]];
				y = yval[mvs[ptr]];
				if(x-4 < xmin)xmin = x-4;
				if(x+4 > xmax)xmax = x+4;
				if(y-4 < ymin)ymin = y-4;
				if(y+4 > ymax)ymax = y+4;
				}
			}
		}
	}



/* fixgralive finds the new alive value for all marked groups
 * and their associated armies 
 * groups with aliveness SMOTHERED and DEAD have already been found
 * first pass finds point eyes, dead group eyes, surrounded
 * territory and the liberties of the army.  If this is enough for 2 eyes,
 * we are done.
 * second pass looks at edgeeyes, and tries to find miai for 2 eyes among
 * connecting to another army, expanding along edge, capturing threatened
 * group, playing a vital eyemaking point;
 * third pass radiates thickness, and judges weak groups based on
 * strength of surrounding groups, and ability to run away.
 */

void fixgralive(void){
	int armylist,armylist2,g,ptr,army;
	armylist = armylist2 = EOL;
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g]){
			gralive[g] &= 31;
			continue;
			}
		if(armylbp[grarmy[g]] != EOL){
			killist(&armylbp[grarmy[g]]);
			}
		if(armynbp[grarmy[g]] != EOL){
			killist(&armynbp[grarmy[g]]);
			}
		if(gralive[g] == SMOTHERED || 
			gralive[g] == DEAD){
			armylibs[grarmy[g]] = grlibs[g];
			cpylist(grlbp[g],&armylbp[grarmy[g]]);
			continue;
			}
		}
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		army = grarmy[g];
		if(armylbp[army] != EOL)continue;
		startalive(army);
		if(armygroups[army] == EOL){
			continue;
			}
		if(armyeyes[army] >= 16){
			if(grthreatened[list[armygroups[army]]])
				newalive(army,UNSETTLED_THREATENED);
			else
				newalive(army,HAS_TWO_EYES);
			}
		else if(armyeyes[army] >= 8 && armyeyespace[army] >= 24){
			if(grthreatened[list[armygroups[army]]])
				newalive(army,UNSETTLED_THREATENED);
			else 
				newalive(army,3);
			}
		else if(armyeyes[army] >= 8 && armyeyespace[army] >= 20){  /* 4 */
			if(grthreatened[list[armygroups[army]]])
				newalive(army,UNSETTLED_THREATENED);
			else
				newalive(army,4);
			}
		else
			 adflist(army,&armylist2);
			
		}
		

	for(ptr = armylist2; ptr != EOL; ptr = links[ptr])
		if(!miaialive(list[ptr])){
			adflist(list[ptr],&armylist);
			}
		/* find groups with miai for two eyes */
	killist(&armylist2);



	radiateterr();  /* radiate territory/thickness from alive and dead stones */
                                /* must be before rn_pot */

	for(ptr = armylist; ptr != EOL; ptr = links[ptr]){
			getarmyrn_pot(list[ptr]);
			getarmywk_pot(list[ptr]);
			}

	for(ptr = armylist; ptr != EOL; ptr = links[ptr]){
		if(armyrn_pot[list[ptr]] == 0)connect_run(list[ptr]);
		}

	for(ptr = armylist; ptr != EOL; ptr = links[ptr])
		semeaialive(list[ptr]); /* find groups that can win semeai */


	for(ptr = armylist; ptr != EOL; ptr = links[ptr])
		weakalive(list[ptr]);  /* evaluate aliveness of weak groups */

	killist(&armylist);
	}


void initarmyalive(int army){
	int i;
	for(i = 0; i < NUMRUN; ++i)
		if(armyrun[army][i] != EOL)
			killist(&armyrun[army][i]);
	if(armyvitalpoints[army] != EOL)
		killist(&armyvitalpoints[army]);
	killist(&armyeyerecs[army]);
	if(armypot[army] != EOL){
		killist(&armypot[army]);
		}
	armyterr[army] = 0;
	armyeyepotential[army] = 0;
	armyeyespace[army] = 0;
	armyrn_pot[army] = 0;
	armywk_pot[army] = 0;
	}


/* startalive makes a liberty list for each army in armylist. It looks
 * at the group neighbors and calculates 
 * armyeyes - eyes from point eyes and dead neighbors
 * armyterr - amount of solid territory army has
 * armyeyespace - eyes from points, dead nbrs, and solid territory
 * it constructs the armyeyerecs and armyvitalpoints lists for each army
 */

void startalive(int army){
	int ptr;
   
	initarmyalive(army);
	nextpot = 1;

	getarmylibs(army);
	
	armyeyes[army] = getnumeyes(army);
	armyeyespace[army] = armyeyes[army];
	
	if(armyeyes[army] >= 16)return;
	
	for(ptr = armyeyerecs[army]; ptr != EOL; ptr = links[ptr]){
		if(eyepot[list[ptr]] != 0)
			mrglist(eyevital[list[ptr]],&armyvitalpoints[army]);
		}
	armyterr[army] = getterr(army);
	armyeyespace[army] += sumeyes[armyterr[army]];
	}


/* getterr finds the territory controlled by these liberties and not
 * already counted as eyes
 * count all territory in center and on edge of board
 * subtract one for each point which can be an eye
 * subtract one for vital point next to open eye
 * subtract one for each point next to vital points
 * subtract 2 for a connection to another army since opponent can play there
 */

int getterr(int army){
	int territory = 0,ptr,s,sn,tmplist = EOL;
	int sn2,ptr2,isconn,cn,eflag,num,c;

	c = grcolor[list[armygroups[army]]];
	for(ptr = armylbp[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];

		isconn = FALSE;
		for(ptr2 = cnbrd[s]; ptr2 != EOL; ptr2 = links[ptr2]){
			cn = list[ptr2];
			if(!PROTCONN(cn))isconn = TRUE;
			}

        	if(ltr1[s] == 0 || ltrgd[s] == 1){ /* center territory */
			eflag = FALSE;
			for(ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = links[ptr2])
				if(lnbf[list[ptr2]][1-c] != 0)eflag = TRUE;
			if(ld[s] >= 4 && eyerec[s] == 0 &&
				!grthreatened[lgr[s]] &&
				gralive[lgr[s]] != DEAD && !isconn && !eflag)
				++territory;
			}

		else if(ltr1[s] != 0){  /* edge territory */
			if(ltrgd[s] == 0 &&
	               	   !grthreatened[lgr[s]] && 
				gralive[lgr[s]] != DEAD){
        	          	territory += ltr1[s];
                	  	if(eyerec[s] != 0){
					--territory;
					addlist(s,&tmplist);
					
					}
				/* don't count the spot with the eye in it */
				else if(isconn){
					--territory;
					addlist(s,&tmplist);
					if(ltr1[s] > 1)--territory;
					}
                  		}
			else if(ltr1[s] > edge[s]*2 && ltrgd[s] <= 4 &&
				!grthreatened[lgr[s]] &&
				gralive[lgr[s]] != DEAD &&
				armysize[grarmy[lgr[s]]] > 1){
        	          	territory += ltr1[s] - edge[s]*2;
				if(ltr1[s] == 9)territory-= 2;
                	  	if(eyerec[s] != 0 || isconn){
					--territory;
					addlist(s,&tmplist);
					}
				}
			}
               	}

	for(ptr = armyvitalpoints[army]; ptr != EOL; ptr = links[ptr]){
		sn = list[ptr];

		/* no territory for vital points */

		if(eyerec[sn] == 0 && ltr1[sn] != 0 && ltrgd[sn] == 0)
			if(addlist(sn,&tmplist)){
				--territory;
				}

		/* no territory for (up to 1) points next to vital points */
		/* points next to vital points that become 3 sided eyes */
		/* are analyzed by getarmytv_pot so they don't count either */

		num = 0;
		for(ptr2 = nblbp[sn]; ptr2 != EOL; ptr2 = links[ptr2]){
			sn2 = list[ptr2];
			if(eyerec[sn2] != 0)continue;  /* already took out eyes */
			if(ltr1[sn2] != 0 && ltrgd[sn2] == 0){
				if(addlist(sn2,&tmplist)){
					/* vital analysis will find this */
					if(lnbn[sn2] == 2)territory--;
					else
						++num;
					}
				}
			else if(ltr1[sn2] == 0 && ld[sn2] >= 4 && ld[sn2] < 9){
				if(addlist(sn2,&tmplist)){
					if(lnbn[sn2] == 2)territory--;
					else
						++num;
					}
				}
			}
		if(num > 1)num = 1;
		territory -= num;
		}
	killist(&tmplist);
	if(territory < 0)territory = 0;
	if(territory > MAXTERR)territory = MAXTERR;
	return(territory);
	}


/* pointeyes looks in the liberties for eyes and adds them up.
 * it also looks for eyes due to nearby dead groups on the edge
 */

int pointeyes(int army){
	int ptr,eyes,s,sn,ldtmp,j,rn,c;
	eyes = 0;
	c = grcolor[list[armygroups[army]]];
	for(ptr = armylbp[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		rn = eyerec[s];
		if(rn != 0 && eyetype[rn] != THRTEYE && 
			addlist(rn,&armyeyerecs[army]))
			eyes += eyeval[rn];
            	if(edge[s] != 1)continue;
			/* eyes not in contact with on edge */
               	j = fdir[s];
               	for(ldtmp = j+2; j < ldtmp; ++j){	
				/* only look along edge */
                  	sn = s + nbr[j];
                  	if(eyerec[sn] != 0 && grcolor[board[sn]] != c &&
                     		ld[sn] == 0 && grnbp[board[sn]] == EOL){
				rn = eyerec[sn];
                     		if(rn != 0 && eyetype[rn] != THRTEYE && 
					addlist(rn,&armyeyerecs[army])){
                        		eyes += eyeval[eyerec[sn]];
                           		}
                        	}
			if(edge[sn] == 0)continue;
                  	sn = sn + nbr[j];
                  	if(eyerec[sn] != 0 && grcolor[board[sn]] != c &&
                     		ld[sn] == 0 && grnbp[board[sn]] == EOL){
				rn = eyerec[sn];
                     		if(rn != 0 && eyetype[rn] != THRTEYE && 
					addlist(rn,&armyeyerecs[army])){
                        		eyes += eyeval[eyerec[sn]];
					}
				}
			}
		}
	if(eyes > 40)eyes = 40;
	return(eyes);
	}


/* getarmynbp finds all the neighboring armies of army and puts them in
 * armynbp[army].
 */

void getarmynbp(int army){
	int ptr,ptr2,i,ldtmp,sn,c;
	c = grcolor[list[armygroups[army]]];
	for(ptr = armygroups[army]; ptr != EOL; ptr = links[ptr]){
		for(ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2])
			addlist(grarmy[list[ptr2]],&armynbp[army]);
		for(ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
			if(ld[list[ptr2]] != NEUTRALLD)continue;
			i = fdir[list[ptr2]];
			for(ldtmp = ldir[i]; i < ldtmp; ++i){
				sn = list[ptr2] + nbr[i];
				if(grcolor[board[sn]] == 1-c)
					addlist(grarmy[board[sn]],&armynbp[army]);
				}
			}
		}
	}
	


/* getarmylibs finds all of the liberties of army and puts them in
 * armylibs[army] and armylbp[army]
 */

void getarmylibs(int army){
	int ptr,libs,g;
	libs = grlibs[list[armygroups[army]]];
	cpylist(grlbp[list[armygroups[army]]],&armylbp[army]);

	for(ptr = links[armygroups[army]]; ptr != EOL; ptr = links[ptr]){
		g = list[ptr];
		libs += mrglist(grlbp[g],&armylbp[army]);
		}
	armylibs[army] = libs;
	}


/* th_run figures out if can run by capturing enemy stone
 *
 */

int th_run(int g){
	int maxnbn=0,totnbn=0,ptr;
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		if(lnbn[list[ptr]] > maxnbn)maxnbn = lnbn[list[ptr]];
		totnbn += lnbn[list[ptr]];
		}
	if(totnbn - maxnbn > 4)return(4);
	if(totnbn - maxnbn > 2)return(2);
	return(0);
	}



/* getarmyth_pot looks at the neighbors of the groups in army.  
 * it finds all the threatened neighbors
 * eye potential comes from li values for THREATENED group or
 * from other army could be connected to.
 */

void getarmyth_pot(int army){
	int ptr,g2,ptr2,s,th_pot,tmplist = EOL;
	int c,i,ldtmp,sn;
	atariflag = FALSE;
	c = grcolor[list[armygroups[army]]];
	for(ptr = armygroups[army]; ptr != EOL; ptr = links[ptr]){
            	g2 = list[ptr];
	      	for(ptr2 = grnbp[g2]; ptr2 != EOL; ptr2 = links[ptr2]){
			if(grthreatened[list[ptr2]]){
				addlist(list[ptr2],&tmplist);
				}
			}
		}

	for(ptr = armylbp[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(ld[s] == NEUTRALLD){
			i = fdir[s];
			for(ldtmp = ldir[i]; i < ldtmp; ++i){
				sn = s + nbr[i];
				if(grcolor[board[sn]] == 1-c && 
				   grthreatened[board[sn]])
					addlist(board[sn],&tmplist);
				}
			}
		}

	for(ptr2 = tmplist; ptr2 != EOL; ptr2 = links[ptr2]){
		if(grlibs[list[ptr2]] == 1)atariflag = TRUE;
		th_pot = 0;
		th_pot += eyepot[eyerec[mvs[grpieces[list[ptr2]]]]];
		th_pot += th_terr(army,list[ptr2]);
		th_pot += th_conns(army,list[ptr2]);
		th_pot += th_run(list[ptr2]);
		if(nextpot < NUMPOTENTIAL-1){
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = THREAT;
			pots[nextpot].pot_val = th_pot;
			pots[nextpot++].pot_where = list[ptr2];
			}
		}
	killist(&tmplist);
	}




int th_terr(int army,int g){
	int ptr,terr = 0,sub,min,eyeflag = FALSE,s,ptr2,c,s2;
	c = grcolor[g];
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		if(ltrgd[list[ptr]] && ltr1[list[ptr]] > 1)
			terr += ltr1[list[ptr]];
		for(ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
			s = list[ptr2];
			if(lnbf[s][c] == 0 && lnbn[s] == 2){
				s2 = list[nblbp[s]];
				if(s2 == list[ptr])
					s2 = list[links[nblbp[s]]];
				if(lnbf[s2][c] == 0 && lnbn[s] == 2)
					eyeflag = 8;
				}
				
			}
		}
	sub = grlibs[g];  /* have to fill liberties */
	min = 5-grsize[g];  /* can add stones up to size 5 */
	if(grlibs[g]-1 < min)min = grlibs[g]-1;  /* or until captured */
	terr -= min+sub;
	if(eyeflag)terr--;
	if(terr < 0)terr = 0;
	return(sumeyes[armyterr[army]+terr] - sumeyes[armyterr[army]] + eyeflag);
	}


/* find eyespace value of connecting to nbrs
 * of g which are not in army
 */

int th_conns(int army,int g){
	int ptr,ceyes,g2,a,c,tmplist = EOL,i,ldtmp;
	ceyes = 0;
	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
		g2 = list[ptr];
		if(gralive[g2] == DEAD || grthreatened[g2])
			continue;
		a = grarmy[g2];
		if(a == army)continue;
		if(!addlist(a,&tmplist))continue;
		ceyes += armyeyes[a] + sumeyes[armyterr[a]];
		}
	c = 1-grcolor[g];
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		if(lnbf[list[ptr]][c] == 0)continue;
		i = fdir[list[ptr]];
		for(ldtmp = ldir[i]; i < ldtmp; ++i){
			g2 = board[list[ptr]+nbr[i]];
			if(grcolor[g2] != c)continue;
			if(grarmy[g2] == army)continue;
			if(!addlist(grarmy[g2],&tmplist))continue;
			ceyes += armyeyes[grarmy[g2]] + sumeyes[armyterr[grarmy[g2]]];
			}
		}
	killist(&tmplist);
	return(ceyes);
	}


/* return number of eyes in army.  Update armyeyerecs to point at eyes */

int getnumeyes(int army){
	int eyes;
	eyes = deadgroups(army);
	if(eyes < 16)eyes += pointeyes(army);
	return(eyes);
	}

/* deadgroups looks at the dead groups of this army and returns the number
 * of eyes
 */

int deadgroups(int army){
	int ptr,g,g2,ptr2,eyespace,rn;
	eyespace = 0;
      	for(ptr = armygroups[army]; ptr != EOL; ptr = links[ptr]){
            	g = list[ptr];
  	    	for(ptr2 = grnbp[g]; ptr2 != EOL; ptr2 = links[ptr2]){
			g2 = list[ptr2];
			if(gralive[g2] != DEAD)continue;
			rn = eyerec[mvs[grpieces[g2]]];
			if(rn != 0){
				if(addlist(rn,&armyeyerecs[army]))
					eyespace += eyeval[rn];
				}
			}
         	}
	if(eyespace > 40)eyespace = 40;
	return(eyespace);
	}




/* get values for eyepotential for army for
 * extensions
 * find the outermost liberty along each edge on the 4 line or lower.
 * 
 */


void getarmyex_pot(int army){
	int ptr,lmin,lmax,rmin,rmax,tmin,tmax,bmin,bmax,points[8],i;
	int s,x,y;
	if(armysize[army] == 1 && armylibs[army] < 3)return;
	    /* can't extend if in contact fight, no time */
	if(grthreatened[list[armygroups[army]]])return;
	   /* no time to extend if threatened */

	/* find the outsidemost liberties along each edge.  In case of
		tie take liberty closest to edge */

	for(i = 0; i < 8; ++i)points[i] = NOSQUARE;
	tmax = bmax = lmax = rmax = -1;
	tmin = bmin = lmin = rmin = 100;
	for(ptr = armylbp[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(edge[s] > 4 || edge[s] == 0)continue;

		x = xval[s];
		y = yval[s];
		if(x < 4){
			if(y > lmax && y < boardsize-2 || 
				y == lmax && edge[s] < edge[points[0]]){
				lmax = y;
				points[0] = s;
				}
			if(y < lmin && y > 1 ||
				y == lmin && edge[s] < edge[points[1]]){
				lmin = y;
				points[1] = s;
				}
			}
		else if(x > boardsize - 5){
			if(y > rmax && y < boardsize-2 ||
				y == rmax && edge[s] < edge[points[2]]){
				rmax = y;
				points[2] = s;
				}
			if(y < rmin && y > 1 ||
				y == rmin && edge[s] < edge[points[3]]){
				rmin = y;
				points[3] = s;
				}
			}
		if(y < 4){
			if(x > tmax && x < boardsize-2 ||
				x == tmax && edge[s] < edge[points[4]]){
				tmax = x;
				points[4] = s;
				}
			if(x < tmin && x > 1 ||
				x == tmin && edge[s] < edge[points[5]]){
				tmin = x;
				points[5] = s;
				}
			}
		else if(y > boardsize - 5){
			if(x > bmax && x < boardsize-1 ||
				x == bmax && edge[s] < edge[points[6]]){
				bmax = x;
				points[6] = s;
				}
			if(x < bmin && x > 1 ||
				x == bmin && edge[s] < edge[points[7]]){
				bmin = x;
				points[7] = s;
				}
			}
		}
	if(lmax != -1)check_ex(points[0],boardsize,1,army,DOWN,UP);
	if(lmin != 100)check_ex(points[1],-boardsize,1,army,UP,DOWN);
	if(rmax != -1)check_ex(points[2],boardsize,-1,army,DOWN,UP);
	if(rmin != 100)check_ex(points[3],-boardsize,-1,army,UP,DOWN);
	if(tmax != -1)check_ex(points[4],1,boardsize,army,RIGHT,LEFT);
	if(tmin != 100)check_ex(points[5],-1,boardsize,army,LEFT,RIGHT);
	if(bmax != -1)check_ex(points[6],1,-boardsize,army,RIGHT,LEFT);
	if(bmin != 100)check_ex(points[7],-1,-boardsize,army,LEFT,RIGHT);
	}

int undercut(int s,int dir,int dir2,int c,int udir,int udir2);

/* check_ex looks for how much eyespace can be added by extending from 
 * s in direction dir.  
 * s is the end liberty on this edge for an army
 *   s is on the 1 thru 4 line
 * Dir2 is direction away from edge of board
 * c is the color of the group extending
 * can't extend from edge
 * can't extend if end liberty is between stone and edge
 * sdir, sdir2 are the square direction for use in sqrbrd etc.
 * sdir points out along the edge and sdir2 points the opposite
 */

void check_ex(int s,int dir,int dir2,int army,int sdir,int sdir2){
	int su,sd,so,suo; /* up, down, out, upand out */
	int c,at,terr,g,two_stone_wall,under_enemy,dist,tmp;
	if(edge[s] <= 3 && ltr1[s] == 0)return;  /* stones between liberty and edge */
	at = armyterr[army];
	c = grcolor[list[armygroups[army]]];
	if(edge[s] == 1 && edge2[s] <= 4)return;
	su = s + dir2;  /* point up from liberty */
	if(edge[s] <= 1)
		sd = s;
	else
		sd = s - dir2;  /* point down from liberty */
	so = s + dir;  /* point out from liberty */
	suo = s + dir + dir2;
	if(grcolor[board[sd]] == c)return; /* lib towards center */
	if(grcolor[board[so]] == c)return; /* inside lib */
	if(edge[so] <= 2 && edge[s] > edge[so])return;  /* don't extend into corner */
	if(grcolor[board[su]] == c){ /* underneath lib, hane for more */
		if(edge[s] < 3)return;
		if(ld[s] == NEUTRALLD)return;
		if(ltrgd[s] >= 8)return;
		if(board[so] != NOGROUP)return;  /* inside lib */
		if(armysize[army] == 1 && armylibs[army] < 3)return;
		g = board[suo];
		if(g == NOGROUP)return;
		if(grlibs[g] == 3 && grsize[g] == 1)
			terr = edge[s] + edge[s] - 1;
		else
			terr = edge[s] - 1;
		if(nextpot < NUMPOTENTIAL-1){
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = EXTEND;
			pots[nextpot].pot_val = sumeyes[at+terr]-sumeyes[at];
			pots[nextpot++].pot_where = s;
			return;
			}
		}

/* outside edge lib */


	if(ld[s] == NEUTRALLD){  
		if(edge[s] <= 1)return;
		if(armysize[army] == 1 && armylibs[army] < 4)return;
		if(grcolor[board[sd]] == 1-c)return;
		if(grcolor[board[sd+dir]] == 1-c)return;
		if(grcolor[board[sd+dir+dir]] == 1-c)return;
		if(grcolor[board[so]] == 1-c)return;
		/* can crawl under */
		terr = terr2[edge[s]];
		if(edge2[so] < edge2[s] && edge2[s] <= 4)++terr;
		/* crawling into corner */

		if(nextpot < NUMPOTENTIAL-1){
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = EXTEND;
			pots[nextpot].pot_val = sumeyes[at+terr]-sumeyes[at];
			pots[nextpot++].pot_where = s;
			}
		}
	else if(ltrgd[s] == 1){
		two_stone_wall = ld[so] == NOLD &&
			grcolor[lgr[su]] == c && ltrgd[su] == 1 && edge[s] < 4;
		if(!two_stone_wall && ld[so] == NOLD && ld[su] == NOLD &&
		   ld[su-dir] > 4)two_stone_wall = TRUE;
		under_enemy = grcolor[lgr[so]] == 1-c && ld[so] != NEUTRALLD;
		if(!under_enemy && edge[so] > 1 && grcolor[lgr[so+dir]] == 1-c
		   && ld[so+dir] != NEUTRALLD)under_enemy = TRUE;
		terr = 0;
		if(under_enemy)
			terr = terr4[edge[s]];
		else if(two_stone_wall)
			terr = terr3[edge[s]];
		else if(ld[so] == NOLD)
			terr = terr1[edge[s]];
		if(nextpot < NUMPOTENTIAL-1){
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = EXTEND;
			pots[nextpot].pot_val = sumeyes[at+terr]-sumeyes[at];
			pots[nextpot++].pot_where = s;
			}
		}
	else if(ltrgd[s] == 3 || ltrgd[s] == 2){
		terr = terr4[edge[s]];
		if(edge2[so] < edge2[s] && edge2[s] <= 4)++terr;
		if(nextpot < NUMPOTENTIAL-1){
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = EXTEND;
			pots[nextpot].pot_val = sumeyes[at+terr]-sumeyes[at];
			pots[nextpot++].pot_where = s;
			}
		}
	else if(ltrgd[s] >= 4 && ltrgd[s] <= 7 && ld[sd] != NEUTRALLD){  /* undercut one way */
		if(edge[s] <= 1)return;
		dist = ucutdist(s,sdir,-dir2,c);
		tmp = ucutdist(s,sdir2,-dir2,c);  /* how far undercut from behind? */
		if(tmp < 5)dist -= 6-tmp;
		if(undercut(s-dir2,-dir2,dir,c,sdir,sdir2))dist--;
		    /* undercut from below (not just line stone is on) */
		under_enemy = grcolor[lgr[so]] == 1-c && ld[so] != NEUTRALLD;
		if(!under_enemy && edge[so] > 1 && grcolor[lgr[so+dir]] == 1-c
		   && ld[so+dir] != NEUTRALLD)under_enemy = TRUE;
		if(under_enemy)--dist;
		if(dist < 0)dist = 0;
		terr = terr5[edge[s]][dist];
		if(edge2[so] < edge2[s] && edge2[s] <= 4)++terr; /* into corner */
		if(terr < 0)terr = 0;
		if(nextpot < NUMPOTENTIAL-1){
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = EXTEND;
			pots[nextpot].pot_val = sumeyes[at+terr]-sumeyes[at];
			pots[nextpot++].pot_where = s;
			}
		}
	}

/* what is distance to undercutting stone in direction sdir.  dir is
 * offset to edge of board.  s is starting point.  c is color of group being
 * undercut.  distance returned (1-4) is number of lines between
 * this stone and the undercutting stone.  returns 5 if out of range.
 */

int ucutdist(int s,int sdir,int dir,int c){
	int dist = 4;
	int sn;
	sn = s-dir;
	do {
		sn = sn+dir;
		if(grcolor[board[sqrbrd[sn][sdir]]] == 1-c && dstbrd[sn][sdir]
		   < dist)dist = dstbrd[sn][sdir];
		} while(edge[sn] > 1);
	return(dist+1);
	}



/* return TRUE if the stoneat sg can undercut square sn 
 * at distance dist (number of empty points between sg and sn).  
 * dir2 is amount to add to sn to go towards sg
 */

int canundercut(int sg,int dir2,int sn,int dist){
	int ptr;
	if(gralive[board[sg]] == DEAD)return(FALSE);
	if(dist < 1)return(TRUE);
	if(grsize[board[sg]] == 1 &&
		grlibs[board[sg]] == 2 && lnbn[sg-dir2] == 1 && 
		lnbf[sg-dir2][grcolor[board[sg]]] == 1)return(FALSE);
	        /* can give atari when he pushes */
	if(dist < 2)return(TRUE);
	if(grthreatened[board[sg]])return(FALSE);
	if(lnbf[sg-dir2-dir2][grcolor[board[sg]]] == 0 && 
	   lnbn[sg-dir2] == 1 && lnbf[sg-dir2][grcolor[board[sg]]] == 1){
		if(lnbn[sg-dir2-dir2] == 2)return(FALSE);
		/* can block when he pushes */
		for(ptr = nblbp[sg-dir2-dir2]; ptr != EOL; ptr = links[ptr])
			if(ld[list[ptr]] >= 4 && ld[list[ptr]] <= 8)
				return(FALSE);
		/* can block when he pushes */
		}
	if(lnbf[sg-dir2-dir2][grcolor[board[sg]]] == 0 && 
		lnbn[sg-dir2-dir2] == 2)return(FALSE);
	        /* can atari when he jumps in (or wedges or hanes) */
	if(dist < 3)return(TRUE);
	if(lnbf[sg-dir2-dir2-dir2][grcolor[board[sg]]] == 0 &&
	   lnbn[sg-dir2] == 1 && lnbf[sg-dir2][grcolor[board[sg]]] == 1 &&
		lnbn[sg-dir2-dir2-dir2] == 2)return(FALSE);
	if(lnbf[sg-dir2-dir2-dir2][grcolor[board[sg]]] == 0 && 
		lnbn[sg-dir2-dir2-dir2] == 2)return(FALSE);
	        /* can atari when he jumps in (or wedges or hanes) */
	return(TRUE);
	}



/* return undercut value if square s is undercut
 * 4 if undercut one direction and 8 if undercut two ways
 * c is the color of this square
 * dir is direction to the
 * edge of the board (amount to add).  
 * dir2 is direction along edge of board to left (facing edge)
 * Undercut is if any squares between
 * this square and the edge of the board inclusive have a live 
 * enemy stone within 4 (2 for threatened)
 * lines along the edge.  Or if any squares have a dead or threatened 
 * friendly stone
 * within 2 lines along the edge.
 * udir and udir2 are directions along edge.
 */

int 
undercut(int s,int dir,int dir2,int c,int udir,int udir2){
	int lcount = 0,rcount=0,sn,g,uc,cval,dist;
	sn = s-dir;
	do {
		sn += dir;
		g = board[sqrbrd[sn][udir]];
		dist = dstbrd[sn][udir];
		uc = grcolor[g];
		if((gralive[g] < SMOTHERED || gralive[g] > DEAD) && 
			uc == 1-c && canundercut(sqrbrd[sn][udir],dir2,sn,dist)){
			lcount++;
			}
		if((gralive[g] == SMOTHERED || grthreatened[g]) && uc == c &&
			dist < 2)
			lcount++;
		g = board[sqrbrd[sn][udir2]];
		dist = dstbrd[sn][udir2];
		uc = grcolor[g];
		if((gralive[g] < SMOTHERED || gralive[g] > DEAD) && 
			uc == 1-c && canundercut(sqrbrd[sn][udir2],-dir2,sn,dist)){
			rcount++;
			}
		if((gralive[g] == SMOTHERED || grthreatened[g]) && uc == c &&
			dist < 2)
			rcount++;
		}while(edge[sn] != 0 && (edge[sn] != 1 || edge[sn-dir] != 2));
	cval = 0;
	if(rcount)cval += 4;
	if(lcount)cval += 4;
	return(cval);
	}


/* find all of the territory for army which is undercut once but
 * otherwise good
 */


void getarmyuc_pot(int army){
	int ptr,s,terr,c;
	c = grcolor[list[armygroups[army]]];
	for(ptr = armylbp[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(ltr1[s] == 0 || ltrgd[s] == 0)continue;
		terr = 0;
		if(ltrgd[s] == 1)continue;
		if(ltrgd[s] == 4){
			terr = ltr1[s];
			if(eyerec[s] != 0 || eyevitrec[s] != EOL)--terr;
			}
		else if(ltrgd[s] == 3){
			terr = ltr1[s];
			if(lnbn[s] > 2 || edge[s] == 0)--terr;
			}
		else if(ltrgd[s] == 2 && lnbf[s][1-c] == 1){
			terr = ltr1[s]-1;
			if(terr == 0)continue;
			}
		else if(ltrgd[s] == 7){
			terr = ltr1[s]-1;
			}
		if(eyerec[s] != 0)terr--;  /* don't count eyes as territory */
		if(terr < 0)terr = 0;
		if(nextpot < NUMPOTENTIAL-1){
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = UNDERCUT;
			pots[nextpot].pot_val = terr;
			pots[nextpot++].pot_where = s;
			}
		}
	}


int doubleconnect(int army,int a){
	int ptr,val = 0,g,a2;
	g = list[armygroups[a]];
	for(ptr = grcnp[g]; ptr != EOL; ptr = links[ptr]){
		a2 = grarmy[cngr1[list[ptr]]];
		if(a2 == a)a2 = grarmy[cngr2[list[ptr]]];
		if(a2 == army)continue; 
		val += armyeyespace[a2];
		}
	return(val);
	}

/* return TRUE if can connect single link cn .
 * return the point to make the connection in s
 */


/* figure out the eye potential for connecting to another army
 * don't double count territory that is shared
 */

void getarmycn_pot(int army){
	int ptr,ptr2,cn,a,g1,g2,val;
	int tmplist = EOL; /* list of connection points */
	int c;

	c = grcolor[list[armygroups[army]]];

	/* Find all the connections */

	for(ptr = armygroups[army]; ptr != EOL; ptr = links[ptr]){
		g1 = list[ptr];
		for(ptr2 = grcnp[g1]; ptr2 != EOL; ptr2 = links[ptr2]){
			cn = list[ptr2];
			if(cnprot[cn] != CAN_CONNECT &&
			   cnprot[cn] != SHARED_CONNECT)continue;
			g2 = cngr1[cn];
			if(grcolor[g2] != c)continue;
			if(g2 == g1)g2 = cngr2[cn];
			a = grarmy[g2];
			if(a == army)continue;
			if(gralive[g2] == DEAD)continue;
			if(grthreatened[g2] && !grthreatened[g1])continue;

			/* found army can connect to */

			val = geteyespace(army,a) - armyeyespace[army];
			if(grthreatened[g1])cnthreat(army,g1,g2,cn);
			if(armylibs[a] > 15)val += 4;
			if(armysize[a] == 1 && cntype[cn] == CN_HANE)
				val += doubleconnect(army,a);
			if(val > 40)val = 40;
				
			if(nextpot < NUMPOTENTIAL-1){
				adflist(nextpot,&armypot[army]);
				pots[nextpot].pot_type = CONNECT;
				pots[nextpot].pot_val = val;
				pots[nextpot++].pot_where = cn;
				}
			}
		}
	if(tmplist != EOL)
		killist(&tmplist);
	}


/* if threatened group is actuall unbreakably connected, look to connections
 * of that group
 */

void cnthreat(int army,int g1,int g2,int cn){
	int ptr,s,cn2,g3,val=0;
	if(cncnum[cn] == 0)return;
	if(cncnum[cn] == 1){
		s = list[cnptr[cn]];
		if(lnbf[s][1-grcolor[g1]] != 0)return;
		if(lnbn[s] > 1)return;
		}
	for(ptr = grcnp[g2]; ptr != EOL; ptr = links[ptr]){
		cn2 = list[ptr];
		g3 = cngr1[cn2];
		if(g3 == g2)g3 = cngr2[cn2];
		if(grarmy[g3] == grarmy[g2])continue;
		val = armyeyespace[grarmy[g3]];
		if(val > 16)val = 16;
		if(nextpot < NUMPOTENTIAL-1){
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = CONNECT;
			pots[nextpot].pot_val = val;
			pots[nextpot++].pot_where = cn2;
			}
		}
	}

/* add up the eye space in common between two armies */

int geteyespace(int a1,int a2){
	int rnlist = EOL,ptr,eyes = 0;
	cpylist(armyeyerecs[a1],&rnlist);
	mrglist(armyeyerecs[a2],&rnlist);
	for(ptr = rnlist; ptr != EOL; ptr = links[ptr])
		eyes += eyeval[list[ptr]];
	eyes += sumeyes[armyterr[a1]+armyterr[a2]];
	killist(&rnlist);
	return(eyes);
	}


/* get eye potential for playing on a vital point
 * if the vital point is for an eye or eyes, they get the potential
 * playing in the vital point can add territory.
 * playing in the vital point can add more eyespace
 * if there are multiple vital points for the same eye, only count the best one
 */


void getarmytv_pot(int army){
	int ptr,sn,vterr,eyeflag,ptr2,sn2,poteyeval,color,ptr3,ptr4,rn,bstpot;
	int eyecount,sn3,tmplist = EOL,val;
	color = 1-grcolor[list[armygroups[army]]];  /* enemy color */
	for(ptr = armyeyerecs[army]; ptr != EOL; ptr = links[ptr]){
		rn = list[ptr];
		bstpot = 0;
		poteyeval = eyepot[rn] - eyeval[rn];
		for(ptr2 = eyevital[rn]; ptr2 != EOL; ptr2 = links[ptr2]){
			sn = list[ptr2];
			vterr = 0;
			eyecount = 0;
			for(ptr3 = nblbp[sn]; ptr3 != EOL; ptr3 = links[ptr3]){
				sn2 = list[ptr3]; /* where new eye will be */
				if(lnbf[sn2][color] != 0)continue;
				if(eyerec[sn2] != 0)continue;
				if(inlist(sn2,&armyvitalpoints[army]) &&
				   !inlist(sn2,&eyevital[rn]))continue;
				eyeflag = 0;
				if(lnbn[sn2] == 2 && ld[sn2] != NEUTRALLD){
					eyeflag = 2;
					for(ptr4 = nblbp[sn2]; ptr4 != EOL; ptr4 =
					    links[ptr4]){
						if(list[ptr4] == sn)continue;
						if(lnbf[list[ptr4]][color] != 0)
							eyeflag = 0;
						else if(lnbn[list[ptr4]] < 2)
							eyeflag = 8;
						else if(lnbn[list[ptr4]] == 2){
							eyeflag = 4;
							sn3 = list[nblbp[list[ptr4]]];
							if(sn3 == sn2)
								sn3 = list[links[nblbp[list[ptr4]]]];
							if(lnbf[sn3][color] == 0 &&
							   (lnbn[sn3] <= 2 ||
							    edge[sn2] == 0))
								eyeflag = 8;
							}
						}
					}
				else if(ltr1[sn2] != 0 && ltrgd[sn2] == 0)
					++vterr;
				else if(ltr1[sn2] == 0 && ld[sn2] > 4)
					++vterr;
				eyecount += eyeflag;
				}
			val = eyecount + sumeyes[armyterr[army]+vterr]-sumeyes[armyterr[army]];
			if(val > bstpot)bstpot = val;
			}
		poteyeval += bstpot;
		if(nextpot < NUMPOTENTIAL-1){
			adflist(nextpot,&armypot[army]);
			pots[nextpot].pot_type = VITAL;
			pots[nextpot].pot_val = poteyeval;
			pots[nextpot++].pot_where = rn;
			}
		}
	killist(&tmplist);
	}

/* canrunhere looks at liberty s of army to see if group can run
 * away here.  Value returned is:
 * -1 can't run here
 * 0 - Wide open - friendly influence only 
 * 1 - Wide open - no influence, no enemy stones 
 * 2 - Wide open - friendly and unfriendly influence, more friendly 
 * 3 - Wide open - friendly and unfriendly influence, more unfriendly 
 * 4 - Wide open - unfriendly influence only, but ligh and running away from
 * 5 - Wide open - unfriendly influence only - heavy or running towards
 * 6 - neutral hole can push thru or enemy stone blocking path
 * 7 - short of liberties - can only extend here, not jump
 */

int runval[] = { 6,5,5,2,3,2,1,1 };

int canrunhere(int army,int s){
	int rflag,ptr2,c,ptr,i,ldtmp,sum,flag,dir = 0,oflag,osqr;
	c = grcolor[list[armygroups[army]]];
	if(edge[s] < 3 || lnbn[s] < 2)return(-1);
	if(ld[s] == NEUTRALLD){
		if(lnbn[s] == 2){
			flag = FALSE;
			sum = 0;
			for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
				sum += list[ptr]-s;
				if(lnbn[list[ptr]] == 4)flag = TRUE;
				}
			if(sum != 0 && flag)return(6);
			}
		return(-1);
		}
	rflag = oflag = FALSE;
	osqr = NOSQUARE;
	for(ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = links[ptr2]){
		if(edge[list[ptr2]] >= 3 &&
		   (lnbn[list[ptr2]] == 4 ||
		    lnbn[list[ptr2]] == 3 && grcolor[lgr[list[ptr2]]] == c &&
		    grarmy[lgr[list[ptr2]]] != army)){
			for(ptr = nblbp[list[ptr2]]; ptr != EOL; ptr = links[ptr])
				if(lnbn[list[ptr]] == 4 && edge[list[ptr]] >= 3){
					rflag = TRUE;
					break;
					}
			if(grcolor[board[s+s-list[ptr2]]] == c &&
			   lnbn[list[ptr2]+list[ptr2]-s] == 4){
				oflag = TRUE;
				osqr = list[ptr2];
				}
			}
		}
	if(!rflag)return(-1);
	if(lnbn[s] == 3){
		i = fdir[s];
		for(ldtmp = ldir[i]; i < ldtmp; ++i){
			if(grcolor[board[s+nbr[i]]] == c){
				dir = nbr[i];
				if(grlibs[board[s+nbr[i]]] == 2)
					return(7);
				break;
				}
			}
		}
	if(!oflag)return(6);
	if(lnbf[s][c] == 2)return(6);
	if(rterv[s][1-c] >= MAXRTVAL/3)return(6);
	if(rterv[s][1-c] <= 0 && rterv[osqr][1-c]){
		if(rterv[s][c] > 0 || rterv[osqr][c] > 0)return(0);
		else return(1);
		}
	else if(rterv[s][c] <= 0){
		if(rterv[s-dir][1-c] < rterv[s][1-c]+10 && rterv[s-dir][1-c] < MAXRTVAL/3)return(4);
		return(5);
		}
	else if(rterv[osqr][c] > rterv[osqr][1-c])return(2);
	else return(3);
	}

/* see if army that can't run can connect to army that can run */

void connect_run(int army){
	int ptr,ptr2,c,g,cn;
	c = grcolor[list[armygroups[army]]];
	for(ptr = armygroups[army]; ptr != EOL; ptr = links[ptr]){
		g = list[ptr];
		for(ptr2 = grcnp[g]; ptr2 != EOL; ptr2 = links[ptr2]){
			cn = list[ptr2];
			if(cnprot[cn] == CAN_CONNECT && grcolor[cngr1[cn]] == c &&
			   !grthreatened[cngr1[cn]] && !grthreatened[cngr2[cn]]){
				if(armyrn_pot[grarmy[cngr1[cn]]] > 1 && 
				   grarmy[cngr1[cn]] != army || 
				   armyrn_pot[grarmy[cngr2[cn]]] > 1 &&
				   grarmy[cngr2[cn]] != army)
					armyrn_pot[army]++;
				}
			}
		}
	}

/* get eye potential for running into center 
 */

void getarmyrn_pot(int army){
	int s,rtype,ptr,psum=0;
	for(ptr = armylbp[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		rtype = canrunhere(army,s);
		if(rtype >= 0){
			addlist(s,&armyrun[army][rtype]);
			if(runval[rtype] == 1)psum++;
			else
				armyrn_pot[army] += runval[rtype];
			}
		}
	if(psum > 3)psum = 3;
	armyrn_pot[army] += psum;
	}

/* miaialive figures out life and death for remaining armies
 * it can give aliveness values:
 * 5 - 2 eyes in eyespace plus territory and miai for another
 * 6 - 2 eyes in eyespace plus territory (in danger)
 * 8 -  miai for 2 1/2  eyes 
 * 9 - miai for 2 eyes exactly. (in danger)
 *	connection to group
 *	threatened neighbors
 *	extending for more eyespace
 *	taking a key eyemaking point
 *	defending undercut eyespace
 * return TRUE if determine aliveness of army, FALSE otherwise
 * if it returns FALSE, it sets aliveness to 23 (WEAK_GROUP)
 */


int miaialive(int army){
	int territory,secondbest;


	getarmyuc_pot(army);  /* undercut territory */
	getarmyth_pot(army);  /* threatened group eyes */
	getarmycn_pot(army);  /* connection for eyes */
	getarmyex_pot(army);  /* extend for eyes */
	getarmytv_pot(army);  /* vital eye point for eyes */


	territory = armyterr[army];


	if(territory > MAXTERR)territory = MAXTERR;
	armyeyespace[army] = armyeyes[army] + sumeyes[territory];
	armyeyepotential[army] = totalpot(army);
	if(armyeyespace[army] + armyeyepotential[army] < 16){
		newalive(army,WEAK_GROUP);  /* can't possibly be miai */
		return(FALSE);
		}

/* don't need to do this here 

	best = bestpot(army);
	if(sumpots[armyterr[army]] > best)best = sumpots[armyterr[army]];
	if(best > 64)best = 64;

	if(armyeyespace[army] + best < 16){
		newalive(army,WEAK_GROUP);   can't possibly be miai 
		return(FALSE);
		}

 */

	if(grthreatened[list[armygroups[army]]]){
		newalive(army,WEAK_GROUP);
		return(FALSE);
		}

	if(armyeyespace[army] >= 16){
		if(armyeyespace[army] >= 20 && armysize[army] > 3)newalive(army,5);
		else if(armyeyepotential[army] >= 16)newalive(army,5);
		else if(armylibs[army] >= 15)newalive(army,5);
		else newalive(army,VERY_ALIVE);
		return(TRUE);
		}
	
	secondbest = secondbestpot(army);

      	if(armyeyespace[army] + secondbest >= 24 && armyeyespace[army] >= 8
	   || armyeyespace[army] + secondbest >= 20 && armyeyepotential[army] >= 32){
		newalive(army,8);
		return(TRUE);
            	}

	if(armyeyespace[army] + secondbest >= 16){
		newalive(army,9);
		return(TRUE);
		}

	newalive(army,WEAK_GROUP);  /* for now, make weak groups really weak */
	return(FALSE);
      	}

/* if army moves first, what is most additional eyespace it can get */
/* see if same move can effect more than one potential (except threat) */


int bestpot(int army){
	int ptr,ptr2,best=0,val,terr,t1,t2,connarmy,cn;
	best = 0;
	for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		terr = 0;
		val = 0;
		if(pots[list[ptr]].pot_type == UNDERCUT)
			terr = pots[list[ptr]].pot_val;
		else
			val = pots[list[ptr]].pot_val;
		if(links[armypot[army]] != EOL && pots[list[ptr]].pot_type != THREAT){
			t1 = adpot(army,list[ptr]);
			connarmy = NOARMY;
			if(pots[list[ptr]].pot_type == CONNECT){
				cn = pots[list[ptr]].pot_where;
				if(grarmy[cngr1[cn]] == army)
					connarmy = grarmy[cngr2[cn]];
				else
					connarmy = grarmy[cngr1[cn]];
				}
			for(ptr2 = armypot[army]; ptr2 != EOL; ptr2 = links[ptr2]){
				if(ptr2 == ptr)continue;
				if(pots[list[ptr2]].pot_type == THREAT)continue;
				if(pots[list[ptr2]].pot_type == CONNECT){
					cn = pots[list[ptr2]].pot_where;
					if(grarmy[cngr1[cn]] == connarmy ||
					   grarmy[cngr2[cn]] == connarmy)
						continue;
					}

				t2 = adpot(army,list[ptr2]);
				if(comlist(t1,t2)){
					if(pots[list[ptr2]].pot_type == UNDERCUT)
						terr += pots[list[ptr2]].pot_val;
					else
						val += pots[list[ptr2]].pot_val;
					}
				killist(&t2);
				}
			killist(&t1);
			}

		val += sumeyes[armyterr[army]+terr]-sumeyes[armyterr[army]];

		if(val > best)
			best = val;
		}
	return(best);
	}


	
/* return total eye potential available to army */

int totalpot(int army){
	int ptr, tot, terrt=0;
	tot = sumpots[armyterr[army]];
	for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		if(pots[list[ptr]].pot_type == UNDERCUT)
			terrt += pots[list[ptr]].pot_val;
		else
		tot += pots[list[ptr]].pot_val;
		}
	tot += sumeyes[armyterr[army]+terrt]-sumeyes[armyterr[army]];
	if(tot > 64)tot = 64;
	return(tot);
	}

/* return best extra eyespace can get if opponent moves first */

int secondbestpot(int army){
	int ptr,best,second = 0, bl = EOL, bsize = -1, bpot = -1;
	int bestlist = EOL,val, tmplist,tmp;
	best = 0;  /* value of best potential */


	/* first find the bestpot with the biggest rmpot list */
	for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		val = pots[list[ptr]].pot_val;
		if(pots[list[ptr]].pot_type == UNDERCUT)
			val = sumeyes[armyterr[army]+val]-sumeyes[armyterr[army]];
		
		if(val >= best){
			bl = rmpot(army,list[ptr]);
			if(bl == EOL)tmp = 0;
			else if(links[bl] == EOL)tmp = 1;
			else if(links[links[bl]] == EOL)tmp = 2;
			else tmp = cntlist(&bl);
			if(val == best && (tmp < bsize || tmp == bsize &&
					   list[bl] > list[bestlist])){
				killist(&bl);
				continue;
				}
			best = val;
			bsize = tmp;
			killist(&bestlist);
			bestlist = bl;
			bl = EOL;
			bpot = list[ptr];
			}
		}


	/* now find the biggest pot that doesn't interfere. */

	for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		if(list[ptr] == bpot)continue;
		tmplist = rmpot(army,list[ptr]);
		if(comlist(tmplist,bestlist)){
			killist(&tmplist);
			continue;
			}
		killist(&tmplist);
		val = pots[list[ptr]].pot_val;
		if(pots[list[ptr]].pot_type == UNDERCUT)
			val = sumeyes[armyterr[army]+val]-sumeyes[armyterr[army]];
		if(val > second )
			second = val;
		}
	killist(&bestlist);
	return(second);
	}


/* return TRUE if move at s is potential eye making move for army */

int moveispoteye(int s,int army){
	int tmplist,ptr;
	for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		tmplist = adpot(army,list[ptr]);
		if(inlist(s,&tmplist)){
			killist(&tmplist);
			return(TRUE);
			}
		killist(&tmplist);
		}
	return(FALSE);
	}

/* find all the moves that realize a particular eye potential */

int adpot(int army,int p){
	int rlist=EOL,cn,g,ptr,tmplist=EOL,t2=EOL,s,c,s2,ptr2,rn,neutral;
	switch(pots[p].pot_type){
	      case CONNECT:
		cn = pots[p].pot_where;
		if(cntype[cn] == CN_ONEPOINTJUMP){
			rlist = rmpot(army,p);  /* at least the removing moves */
			s = connect_bamboo(cngr1[cn],cngr2[cn],list[cnptr[cn]]);
			if(s != NOSQUARE)addlist(s,&rlist);
			break;
			}
		if(cntype[cn] == CN_TWOPOINTJUMP){
			rlist = rmpot(army,p);  /* at least the removing moves */
			neutral = ld[list[cnlkptr[cn]]] == NEUTRALLD ||
				ld[list[links[cnlkptr[cn]]]] == NEUTRALLD;
			g = cngr1[cn];
			if(grlibs[g] < 4 && !neutral)
				for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr])
					if(lnbn[list[ptr]] > 1)
					   addlist(list[ptr],&rlist);
			g = cngr2[cn];
			if(grlibs[g] < 4 && !neutral)
				for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr])
					if(lnbn[list[ptr]] > 1)
					   addlist(list[ptr],&rlist);
			break;
			}
		if(cntype[cn] == CN_HALFLARGEKNIGHT){
			canconnlkg(cn,&s,&s2);
			addlist(s,&rlist);
			break;
			}
		rlist = rmpot(army,p);  /* at least the removing moves */

		break;
	      case THREAT:
		g = pots[p].pot_where;
		if(grlibs[g] == 1)addlist(list[grlbp[g]],&rlist);
		else cpylist(grlbp[g],&tmplist);
		t2 = getamoves(g);
		if(t2 != EOL){
			mrglist(t2,&tmplist);
			killist(&t2);
			}
		t2 = approachmoves(g);
		if(t2 != EOL){
			mrglist(t2,&tmplist);
			killist(&t2);
			}
		for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
			if(grthreatened[list[ptr]])
				mrglist(grlbp[list[ptr]],&tmplist);
			}
		
		for(ptr = tmplist; ptr != EOL; ptr = links[ptr]){
			s = list[ptr];
			if(canbecaptured(s,1-grcolor[g],mvs[grpieces[g]],grcolor[g])){
				addlist(s,&rlist);
				}
			}
		killist(&tmplist);
		break;
	      case VITAL:
		rlist = rmpot(army,p);
		c = grcolor[list[armygroups[army]]];
		rn = pots[p].pot_where;
		for(ptr = eyevital[rn]; ptr != EOL; ptr = links[ptr])
			if(board[list[ptr]] == NOGROUP && lnbf[list[ptr]][1-c] == 0)
				for(ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2])
					if(eyerec[list[ptr2]] != rn)
						addlist(list[ptr2],&rlist);
		break;
	      case EXTEND:
		extendforeyes(army,p,&rlist);
		break;
	      default:
		rlist = rmpot(army,p);  /* at least the removing moves */
		}
	return(rlist);
	}

void extendforeyes(int army,int p,int* rlist){
	int s,so,soo,dir2,twostonewall = FALSE,ptr;

	// sm: bugfix: 'so' was used uninitialized
        //so= 0x456789ab;  // bad; confirms uninit
        
        // sm: ack!  don't comment this out!
        so=0;

	dir2 = 0;  /* toward edge */
	s = pots[p].pot_where;
	if(ld[s] != NEUTRALLD){  /* look for higher liberty */
		for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr])
			if(edge[list[ptr]] > edge[s] && board[s] == NOGROUP){
				if((ld[list[ptr]] == 3 ||
				    ld[list[ptr]] == 4) && lnbn[list[ptr]] ==3  &&
				   grcolor[lgr[list[ptr]]] == grcolor[lgr[s]]){
					s = list[ptr];
					twostonewall = TRUE;
					}
				}
		}
	for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
		if(edge[list[ptr]] == edge[s])
			so = list[ptr];
		if(edge[list[ptr]] < edge[s])dir2 = list[ptr]-s;
		}
	if(ld[s] == NEUTRALLD){
		addlist(s,rlist);
		if(edge[s] > 2 && lnbn[so+dir2] == 4)addlist(so+dir2,rlist);
		return;
		}
	else if(lnbn[so] != 4){     // sm: 'so' is uninitialized
		if(lnbn[so+dir2] == 4 && board[so+dir2] == NOGROUP)addlist(so+dir2,rlist);
		else addlist(s,rlist);
		return;
		}
	else if(armylibs[army] < 4)
		addlist(so,rlist);
	else if(edge[so] > 2){
		soo = NOSQUARE;
		for(ptr = nblbp[so]; ptr != EOL; ptr = links[ptr]){
			if(list[ptr] == s)continue;
			if(edge[list[ptr]] == edge[so]){
				soo = list[ptr];
				break;
				}
			}
		if(soo == NOSQUARE)return;
		if(lnbn[soo] == 4){
			addlist(soo,rlist);
			if(twostonewall && edge[soo] > 2 && edge2[soo] > 3 &&
			   lnbn[soo+soo-so] == 4)
				addlist(soo+soo-so,rlist);
			}
		else {
			if(board[soo+dir2] == NOGROUP && lnbn[soo+dir2] == 4)
				addlist(soo+dir2,rlist);
			if(board[so+dir2] == NOGROUP && lnbn[so+dir2] == 4)
				addlist(so+dir2,rlist);
			}
		}
	}


/* find all moves which remove a particular eye potential */

int rmpot(int army,int p){
	int rlist = EOL,ptr;
	switch(pots[p].pot_type){
	      case CONNECT:
		rlist = rmconnect(army,pots[p].pot_where);
		break;
	      case VITAL:
		for(ptr = eyevital[pots[p].pot_where]; ptr != EOL; ptr = links[ptr])
			if(board[list[ptr]] == NOGROUP)
				addlist(list[ptr],&rlist);
			else 
				mrglist(grlbp[board[list[ptr]]],&rlist);
		break;
	      case UNDERCUT:
		rlist = blockuc(army,pots[p].pot_where);
		break;
	      case THREAT:
		cpylist(grlbp[pots[p].pot_where],&rlist);
		break;
	      case EXTEND:
		rlist = blockextend(army,pots[p].pot_where);
		break;
		}
	
	return(rlist);
	}


int rmconnect(int army,int cn){
	int rlist = EOL,s,s1,s2,xv,yv;
	if(cntype[cn] == CN_DIAGONAL || cntype[cn] == CN_HANE || cntype[cn] == CN_ONEPOINTJUMP)
		cpylist(cnptr[cn],&rlist);
	else if(cntype[cn] == CN_HALFKNIGHT){
		cpylist(cnlkptr[cn],&rlist);
		canconnlink(cn,&s);
		addlist(s,&rlist);
		}
	else if(cntype[cn] == CN_TWOPOINTJUMP)
		cpylist(cnlkptr[cn],&rlist);
	else if(cntype[cn] == CN_THREEPOINTJUMP){
		cpylist(cnllptr[cn],&rlist);
		addlist((list[cnllptr[cn]]+list[links[cnllptr[cn]]])/2,
			&rlist);
		}
	else if(cntype[cn] == CN_KNIGHTSMOVE){
		s1 = list[cnlkptr[cn]];
		s2 = list[links[cnlkptr[cn]]];
		xv = xval[s2]-xval[s1];
		if(xv == -1){
			addlist(s1+boardsize,&rlist);
			addlist(s1+boardsize-1,&rlist);
			}
		else if(xv == 1){
			addlist(s1+boardsize,&rlist);
			addlist(s1+boardsize+1,&rlist);
			}
		else if(xv == 2){
			addlist(s1+1,&rlist);
			addlist(s1+boardsize+1,&rlist);
			}
		else if(xv == -2){
			addlist(s2-boardsize+1,&rlist);
			addlist(s2+1,&rlist);
			}
		
		}
	else if(cntype[cn] == CN_LARGEKNIGHT){
		s1 = list[cnllptr[cn]];
		s2 = list[links[cnllptr[cn]]];
		xv = xval[s2]-xval[s1];
		yv = yval[s2]-yval[s1];
		if(yv == 3)s1 += boardsize;
		if(xv == -1)s1--;
		if(xv == 3)s1++;
		if(xv == -3)s1 -= 2;
		addlist(s1,&rlist);
		addlist(s1+1,&rlist);
		addlist(s1+boardsize,&rlist);
		addlist(s1+1+boardsize,&rlist);
		}
	else if(cntype[cn] == CN_HALFLARGEKNIGHT){
		canconnlkg(cn,&s,&s2);
		addlist(s,&rlist);
		addlist(s2,&rlist);
		addlist(list[cnllptr[cn]],&rlist);
		}
	else if(cntype[cn] == CN_UNKNOWN){
		cpylist(cnptr[cn],&rlist);
		mrglist(cnlkptr[cn],&rlist);
		mrglist(cnllptr[cn],&rlist);
		}
	return(rlist);
	}


int blockextend(int army,int s){
	int rlist = EOL,ptr,ptr2,c,flag;
	c = grcolor[list[armygroups[army]]];
	if(ld[s] == NEUTRALLD){
		addlist(s,&rlist);
		if(edge[s] >= 3){
			for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr])
				if(edge[list[ptr]] < edge[s])
					addlist(list[ptr],&rlist);
			}
		}
	else for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
		if(lnbf[list[ptr]][1-c] != 0){
			flag = FALSE;
			for(ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = links[ptr2])
				if(edge[list[ptr2]] > 1 && edge[list[ptr2]] < edge[s]){
					addlist(list[ptr2],&rlist);
					flag = TRUE;
					break;
					}
			if(!flag)addlist(s,&rlist);
			}
		if(edge[list[ptr]] == edge[s]){
			if(lnbn[list[ptr]] == 4){
				addlist(list[ptr],&rlist);
				if(lnbn[list[ptr]+list[ptr]-s] == 4 &&
				   edge[list[ptr]+list[ptr]-s] == edge[s])
					addlist(list[ptr]+list[ptr]-s,&rlist);
				}
			}
		}
	return(rlist);
	}

int bugfixIndex(int index);    // sm:

int blockuc(int army,int s){
	int ptr,ptr2,rlist = EOL,c,crawl,so,sd,i,ldtmp,sn;
	c = grcolor[list[armygroups[army]]];
	if(ltrgd[s] == 4 || ltrgd[s] == 5)
		for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
			if(edge[list[ptr]] == edge[s] && edge2[s] < 4 &&
			   edge2[list[ptr]] < edge2[s])
				findblock(s,list[ptr],&rlist,c);
			if(edge[list[ptr]] >= edge[s])continue;
			findblock(s,list[ptr],&rlist,c);
			}
	else if(ltrgd[s] == 3){
		i = fdir[s];
		for(ldtmp = ldir[i]; i < ldtmp; ++i){
			sn = s + nbr[i];
			if(board[sn] == NOGROUP)continue;
			if(grlibs[board[sn]] != 2)continue;
			for(ptr = grlbp[board[sn]]; ptr != EOL; ptr = links[ptr])
				if(lnbn[list[ptr]] > 1)addlist(list[ptr],&rlist);
			}
		if(lnbf[s][c] == 1 && grlibs[lgr[s]] == 2)
			addlist(s,&rlist);
		for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
			if(edge[list[ptr]] < edge[s])continue;
			if(lnbf[list[ptr]][1-c] != 0){
				addlist(list[ptr],&rlist);
				if(lnbn[list[ptr]] < 3)
					addlist(s,&rlist);
				}
			if(lnbn[list[ptr]] == 4)addlist(list[ptr],&rlist);
			}
		}
	if(rlist == EOL){
		if(ld[s] == NEUTRALLD){
			crawl = FALSE;
			so = sd = NOSQUARE;
			for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
				if(edge[list[ptr]] == edge[s]){
					so = list[ptr];
					crawl = TRUE;
					}
				if(edge[list[ptr]] < edge[s]){
					sd = list[ptr];
					}
				}
			if(lnbf[s][c] > 1)addlist(s,&rlist);
			else if(crawl){
				if(lnbn[so] == 3 && lnbf[so][c] == 1 && so != NOSQUARE)
					addlist(so,&rlist);
				else addlist(s,&rlist);
				}
			else if(sd != NOSQUARE && lnbf[s][c] > 1)addlist(sd,&rlist);
			else addlist(s,&rlist);
			}
		if(grthreatened[lgr[s]])
			addlist(s,&rlist);
		for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
			if(edge[list[ptr]] > edge[s] &&
			   (edge2[s] > 3 || edge2[list[ptr]] != edge2[s]))continue;
			if(lnbf[list[ptr]][1-c] != 0){
				addlist(list[ptr],&rlist);
				if(lnbn[list[ptr]] < 3)
					addlist(s,&rlist);
				}
			else if(board[ bugfixIndex(list[ptr]+list[ptr]-s) ] == NOGROUP){
				if(lnbf[ bugfixIndex(list[ptr]+list[ptr]-s) ][1-c] != 0)
					addlist(list[ptr],&rlist);
				}
			else for(ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2])
				if(edge[list[ptr2]] > edge[s] && lnbn[list[ptr2]] > 2)
					addlist(list[ptr2],&rlist);
			}
		}
	return(rlist);
	}


void findblock(int s,int sn,int* rlist,int c){
	int dir,sdir,sn2,g;
	dir = sn-s;
	sn = s-dir;
	do {
		sn += dir;
		for(sdir = 0; sdir < 4; sdir++){
			g = board[sqrbrd[sn][sdir]];
			if(grcolor[g] == 1-c){
				sn2 = sn + brddir[sdir]*dstbrd[sn][sdir];
				addlist(sn2,rlist);
				if(lnbf[sn2][c] == 0 && dstbrd[sn][sdir] >= 1)
					addlist(sn2-brddir[sdir],rlist);
				}
			if(grcolor[g] == c && grthreatened[g])
				mrglist(grlbp[g],rlist);
			}
		} while(edge[sn] > 1);
	}


/* update armyweak potential with the number of weak adjacent armies that
 * are not threatened
 */

void getarmywk_pot(int army){
	int ptr,g;
	if(armynbp[army] == EOL)getarmynbp(army);
	for(ptr = armynbp[army]; ptr != EOL; ptr = links[ptr]){
		g = list[armygroups[list[ptr]]];
		if(gralive[g] <= WEAK_GROUP &&
				gralive[g] > ALIVE)
			armywk_pot[army]++;
		}
	}

/* isseki returns TRUE if army is alive as a seki */

int isseki(int army){
	int g,g2,ptr,libs[2],lib;

	g = list[armygroups[army]];
	
	if(grthreatened[g])return(FALSE);
	if(grcnp[g] != EOL)return(FALSE); /* no connections to other groups */
	if(grlibs[g] != 2)return(FALSE);  /* must have exactly two liberties */
	libs[0] = list[grlbp[g]];
	libs[1] = list[links[grlbp[g]]];

	if(lnbf[libs[0]][1-grcolor[g]] == 0 &&
		lnbf[libs[1]][1-grcolor[g]] == 0)return(FALSE);
	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
		g2 = list[ptr];
		if(grlibs[g2] != 2 || grcnp[g2] != EOL)continue;
		if(grthreatened[g2])return(FALSE);
		if(gralive[g2] < 7)continue;
		lib = list[grlbp[g2]];
		if(lib != libs[0] && lib != libs[1] && lnbf[lib][grcolor[g]] != 0)continue;
		lib = list[links[grlbp[g2]]];
		if(lib != libs[0] && lib != libs[1] && lnbf[lib][grcolor[g]] != 0)continue;
		return(TRUE);
		}
	return(FALSE);
	}




/* return number of liberties (min and max) this army has in a semeai.
 * add one for protected liberties and places where can run away.
 * add liberties for adjacent threatened groups
 * min is minimum number of libs if enemy moves first.
 * max is max number of libs if I move first.
 */

void semeailibs(int a,int* min,int* max){
	int ptr,ptr2,cn,maxconn = 0,libs,thlibs,g,rnlibs = 0;
	int cnt = 0,apr = 0,c,tmp,conlist = EOL,i;
	c = grcolor[list[armygroups[a]]];
	for(ptr = armylbp[a]; ptr != EOL; ptr = links[ptr]){
		if(lnbn[list[ptr]] <= 1 && lnbf[list[ptr]][1-c] == 0)apr++;
		if(lnbn[list[ptr]] >= 2){
			tmp = lnbn[list[ptr]]-1-comlist(nblbp[list[ptr]],armylbp[a]);
			if(tmp > 0)cnt += tmp;
			}
		}
	for(ptr = armygroups[a]; ptr != EOL; ptr = links[ptr])
		for(ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
			cn = list[ptr2];
			if(PROTCONN(cn)){  /* connection within army */
				if(cnptr[cn] != EOL)
					mrglist(cnptr[cn],&conlist);
				else if(cnlkptr[cn] != EOL)
					mrglist(cnlkptr[cn],&conlist);
				else
					mrglist(cnllptr[cn],&conlist);
				}
			if(cnprot[cn] != CAN_CONNECT)continue;
			libs = 0;
			if(grarmy[cngr1[cn]] != a)libs = armylibs[grarmy[cngr1[cn]]]-1;
			else if(grarmy[cngr2[cn]] != a)libs = armylibs[grarmy[cngr2[cn]]]-1;
			if(libs > maxconn)maxconn = libs;
			}
	thlibs = 0;
	if(armynbp[a] == EOL)getarmynbp(a);
	for(ptr = armynbp[a]; ptr != EOL; ptr = links[ptr]){
		g = list[armygroups[list[ptr]]];
		if(!grthreatened[g])continue;
		thlibs += grsize[g];
		if(grlibs[g] == 1)thlibs += lnbn[list[grlbp[g]]];
		if(armynbp[list[ptr]] == EOL)getarmynbp(list[ptr]);
		for(ptr2 = armynbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
			if(list[ptr2] == a)continue;
			thlibs += armylibs[list[ptr2]];
			}
		}

	for(i = 0; i <= 5; ++i){
		if(armyrun[a][i] != EOL){
			rnlibs += 3; 
			}
		}

	*min = armylibs[a] - cntlist(&conlist);  /* may have to fill all
						    internal connections */
	if(*min < 2)*min = 2;
	*max = armylibs[a] + cnt + apr + maxconn + thlibs + rnlibs;
	killist(&conlist);
	}



/* return result of semeai between a1 and a2. (can a1 win the semeai)
 * 0 is unsettled result (a1 can win)
 * 1 is a1 wins (a1 has more liberties and can get two eyes from capture and
 *               a2 can't make two eyes)
 * 2 is a2 wins (or a1 can't win)
 * 3 is seki result
 *
 * adjust number of liberties ahead needed depending on how ahead we are
 */

int semeai_result(int a1,int a2){
	int l1min,l1max,l2min,l2max,l3,esc1,esc2;
	esc1 = armyrn_pot[a1];
	esc2 = armyrn_pot[a2];
	if(armyeyespace[a2] + armyeyepotential[a2] >= 16)return(2);
	if(esc2 >= 6)return(2);  /* can't capture if can run away */
/*	if(esc1 && esc2)return(0);  */
	        /* if both can run who knows what could happen */

	semeailibs(a1,&l1min,&l1max);
	semeailibs(a2,&l2min,&l2max);
	l3 = comlist(armylbp[a1],armylbp[a2]);
	if(armyeyespace[a1] < 8 && armyeyespace[a2] < 8){  /* no eyes case */
		if(l3 <= 1){
			if(l1min > l2max && !esc2)return(1);
			if(l2min > l1max && !esc1)return(2);
			return(0); /* cant be seki */
			}
		if(l1min >= l2max+l3 && !esc2)return(1);
		if(l2min >= l1max+l3 && !esc1)return(2);
		if(l1max >= l2min+13)return(0);
		if(l2max >= l2min+l3)return(0);
		if(esc1 || esc2)return(0);
		return(3); /* seki */
		}
	if(armyeyespace[a1] >= 8 && armyeyespace[a2] >= 8){  /* both an eye */
		if(l3 == 0){
			if(l1min > l2max && !esc2)return(1);
			if(l2min > l1max && !esc1)return(2);
			return(0); /* cant be seki */
			}
		if(l1min > l2max+l3 && !esc2)return(1);
		if(l2min > l1max+l3 && !esc1)return(2);
		if(l1max > l2min+13)return(0);
		if(l2max > l2min+l3)return(0);
		if(esc1 || esc2)return(0);
		return(3); /* seki */
		}
	if(armyeyespace[a1] >= 8 && armyeyespace[a2] < 8){  
		if(l1min >= l2max - l3 && !esc2)return(1);
		if(l1max < l2min - l3 && !esc1)return(2);
		return(0);
		}
	if(armyeyespace[a1] < 8 && armyeyespace[a2] >= 8){  
		if(l2min >= l1max - l3 && !esc1)return(2);
		if(l2max < l1min - l3 && !esc2)return(1);
		return(0);
		}
	return(2);
	}


/* army can win a semeai and get two eyes while doing it */

int win_semeai(int army){
	int ptr,g,eyes,maxeyes;
	if(armynbp[army] == EOL)getarmynbp(army);
	for(ptr = armynbp[army]; ptr != EOL; ptr = links[ptr]){
		g = list[armygroups[list[ptr]]];

		if(gralive[g] < ALIVE || gralive[g] > WEAK_GROUP)continue;
	        if(armyeyespace[list[ptr]] + armyeyepotential[list[ptr]] >= 16)
			continue;
		if(armyrn_pot[list[ptr]])continue;
		if(semeai_result(army,list[ptr]) != 1)continue;
		eyes = armyeyespace[army];
		eyes += eyesifcapture(g,army,&maxeyes);
		if(eyes >= 16)return(TRUE);
		}
	return(FALSE);
	}

/* army is in (one or more) semeai and we don't know who will win and 
 * if army wins it
 * gets two eyes
 */

int uns_semeai(int army){
	int ptr,g,eyes,maxeyes;
	eyes = armyeyespace[army];
	if(armynbp[army] == EOL)getarmynbp(army);
	for(ptr = armynbp[army]; ptr != EOL; ptr = links[ptr]){
		g = list[armygroups[list[ptr]]];
		if(gralive[g] <= ALIVE || gralive[g] > WEAK_GROUP)continue;
		if(semeai_result(army,grarmy[g]) == 2)continue;
		eyesifcapture(g,army,&maxeyes);
		eyes += maxeyes;
                }
        if(eyes >= 16)return(TRUE);
	return(FALSE);
	}

/* see if group g is cutting off army from eyes 
 * return minimum number of eyes from capturing group
 * return maxeyes as maximum number of eyes from capture
 */

int eyesifcapture(int g,int army,int* maxeyes){
	int ptr,eyes = 0,a2,size;
	*maxeyes = 0;
	if(grthreatened[g]){
		eyes = eyeval[eyerec[mvs[grpieces[g]]]];
		if(eyes < 16){
			for(ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr])
				if(eyesdiags[mvs[ptr]] != EOL)*maxeyes += 8;
			}
		}
	else {
		size = armysize[grarmy[g]] + armylibs[grarmy[g]];
		if(size > 6)eyes = 16;
		else if(size > 2)eyes = 8;
		if(size < 7){
			for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr])
				if(ld[list[ptr]] != NEUTRALLD)
					size++;
			if(size > 6)*maxeyes += 8;
			}
		
		}
	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
		a2 = grarmy[list[ptr]];
		if(a2 == army)continue;
		eyes += armyeyespace[a2];
		if(armylibs[a2] > 3)*maxeyes += 8;
		if(armylibs[a2] > 6)*maxeyes += 8;
		}
	*maxeyes += eyes;
	return(eyes);
	}


/* semeaialive finds all the groups that are alive in semeai or seki
 * 10 alive because wins semeai
 * 7 Alive in Seki
 */

void semeaialive(int army){
	if(isseki(army))
		newalive(army,SEKI);
        else if(armywk_pot[army] && !grthreatened[list[armygroups[army]]] &&
		win_semeai(army))
                newalive(army,WINS_SEMEAI);
	}


/* is army connected to group that has miai for life? */

int conntoalive(int army){
	int ptr,cn,g;
	for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		if(pots[list[ptr]].pot_type != CONNECT)continue;
		cn = pots[list[ptr]].pot_where;
		g = cngr1[cn];
		if(grarmy[g] == army)g = cngr2[cn];
		if(gralive[g] <= ALIVE)
			return(TRUE);
		}
	return(FALSE);
	}


/* look at armies that are not alive or dead and figure out how
 * weak they are.  Look at ability to run away, and find sekis
 * It can give aliveness values from
 * 22 - No eyespace or potential and nbrs all alive 
 * 21 - probably dead some eye potential, nbrs alive
 * 20 - in semeai loses 
 * 15-19 - some eye potential, weak 
 *   (19 surrounded, some eye potential)
 *   (18 surrounded, lots of eye potential)
 *   (17 can run a little)
 *   (16 can run away no eyes)
 *   (15 can run away and has one eye)
 *     needs two moves to live 
 * 12-14 unsettled - can make two eyes if moves first
 *   (14 in a semeai. need to capture nbr to live)
 *   (13 surrounded, can live or die) 
 *   (12 can live or run) 
 * if group is tactically threatened, it can't be more alive than 11
 */

void weakalive(int army)  {
	int gl,libs,best;
	if(gralive[list[armygroups[army]]] <= ALIVE)return;
	libs = armylibs[army];
	if(libs > 5)libs = 5;
	best = bestpot(army);
	if(sumpots[armyterr[army]] > best)best = sumpots[armyterr[army]];
	if(conntoalive(army) || armyeyespace[army] + best >= 16 ||
	   armyeyespace[army] + armyeyepotential[army] >= 32){ 
		/* can live in one move */
		if(armyrn_pot[army] > 4 || armyrn_pot[army] == 4 && 
		   armyeyespace[army] + armyeyepotential[army] >= 24)
	            	gl = UNSETTLED_RUN;
		else 
			gl = UNSETTLED_DEAD;
		}
	else if(armyrn_pot[army] > 4){
		if(armyeyespace[army] >= 8)
			gl = WEAK_RUN_EYE;
		else
			gl = WEAK_RUN;
			}
	else if(armywk_pot[army] && uns_semeai(army))
		gl = SEMEAI;
	else if(armyrn_pot[army] > 0 && (armyeyespace[army] + armyeyepotential[army] >= 4 || armywk_pot[army] || armyrn_pot[army] > 2))
		gl = WEAK_LIMP;
	else if(armyeyespace[army] + armyeyepotential[army] >= 12)
		gl = WEAK_POTENTIAL;
	else if(armywk_pot[army] && armyeyespace[army]+armyeyepotential[army] >= 4 || armyrn_pot[army] > 0)
		gl = WEAK;
        else if(armywk_pot[army])gl = LOSE_SEMEAI;
	else if(armyeyespace[army] >= 8 ||
		armyeyepotential[army] >= 8)gl = 21;
	else gl = 22;
	newalive(army,gl);
	}

/* give army army the new aliveness value, alive
 */

void newalive(int army,int alive){
	int ptr,g2;

      	for(ptr = armygroups[army]; ptr != EOL; ptr = links[ptr]){
         	g2 = list[ptr];

         	gralive[g2] &= 31;

/*
		if(gralive[g2] > WEAK && alive <= WEAK || gralive[g2] <= WEAK && alive > WEAK)
			for(ptr2 = grpieces[g2]; ptr2 != -1; ptr2 = mvnext[ptr2])
				if(lnbn[mvs[ptr2]] != 0)expandterhd(mvs[ptr2]);
*/
         	if(gralive[g2] != alive){
            		pscr = pscr + (pfac[alive] - pfac[gralive[g2]]) *
				grsize[g2] * cfac[grcolor[g2]];
            		gralive[g2] = alive;
			if(alive != WEAK_GROUP)adflist(g2,&chgrp);
            		}
         	}
	}

  
/* check g for connecttions to other groups than g2, see if throw
 * in in that connection affects this one
 */


int otherweakconn(int g,int g2){
	int cn,ptr,s;
	if(grlibs[g] >= 4)return(FALSE);
	for(ptr = grcnp[g]; ptr != EOL; ptr = links[ptr]) {
		cn = list[ptr];
		if(cngr1[cn] == g2 || cngr2[cn] == g2)continue;
		if(cncnum[cn] > 1)continue;
		if(cncnum[cn] == 0 && cnlknum[cn] > 1)return(TRUE);
		s = list[cnptr[cn]];
		if(s <= NUMSQUARES && lnbn[s] > 1)return(TRUE);
		}
	return(FALSE);
	}


/* check connection at s between g and g2 to see if is is unbreakable.
 * c is color of g and g2
 * This is the only connection between the groups 
 * return true if connection unbreakable
 * need more work on shared threatened group (shared_connect)
 */

int cnoneconn(int g,int g2,int c,int cn){
   int sum,i,sn,dir,dir2,ldtmp,s;
   int ptr3,g3;
   int one_point_jump;
   int thr_friendly,dead_friendly_group,tmplist;


    /* is it a one point jump */
   s = list[cnptr[cn]];

   thr_friendly = 0;
   sum = 0;
   i = fdir[s];
   dir = 0;
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      sn = s + nbr[i];
      if(board[sn] == g){
         dir2 = nbr[i];		/* direction to friendly stone */
         }
      if(board[sn] == g || board[sn] == g2){
         sum += nbr[i];
         }
      else if(grcolor[board[sn]] == c && grthreatened[board[sn]]){
         thr_friendly++;
         dead_friendly_group = board[sn];
         }
      else if(grcolor[board[sn]] == 1-c)dir = nbr[i];	/* dir to enemy */
      else if(board[sn] == NOGROUP && dir == 0)dir = nbr[i];
      }
   one_point_jump = sum == 0;
   if(one_point_jump)cntype[cn] = CN_ONEPOINTJUMP;
   else cntype[cn] = CN_HANE;

   if(grthreatened[g] || grthreatened[g2])return(CAN_CONNECT);
   if(grcolor[lgr[s]] == 1-c && gralive[lgr[s]] == DEAD) 
      return(AJI_CONNECT);	/* geta */
   if(thr_friendly == 2)return(CAN_CONNECT);
   if(thr_friendly){
	   if(lnbn[s] != 0)return(CAN_CONNECT);
	   if(thr_friendly && grsize[dead_friendly_group] == 1 && 
	      grlibs[dead_friendly_group] == 1)
		   return(CANT_CONNECT); /* ko */
	   return(CAN_CONNECT);
	   }

   if(one_point_jump)return(cnonepoint(g,g2,c,cn,s,dir,dir2));

   /* look for common adjacent threatened group */

   tmplist = EOL;
   andlist(grnbp[g],grnbp[g2],&tmplist);
   for(ptr3 = tmplist; ptr3 != EOL; ptr3 = links[ptr3]){
      g3 = list[ptr3];
      if(grthreatened[g3] && list[grlbp[g3]] != s){ 
			/*should look at all libs of g3, not just first */
         killist(&tmplist);
         cntype[cn] = CN_THREAT;
         return(AJI_CONNECT);
         }
      }
   killist(&tmplist);

   if(cntplyhere(s,c,NUMGROUPS+cn)){
      return(AJI_CONNECT);
      }

/* try to capture the opposing hane stone quickly*/
   g3 = board[s+sum];
   if(grcolor[g3] == 1-c &&
      lnbn[s] == 2 && grlibs[g] > 2 && grlibs[g2] > 2 &&
      grsize[g3] == 1 && grlibs[g3] == 2 && playlevel >= 5)
      if(iscaptured(g3,9,quicklevel,quicklibs,c,NUMGROUPS+cn))return(AJI_CONNECT);
   return(CAN_CONNECT);
   }


/* dir is to enemy (or empty square if no enemy), dir2 is to friend */

int cnonepoint(int g,int g2,int c,int cn,int s,int dir,int dir2){
	int enemy_on_diag,two_stone_wall,conn_with_peep,two_lib_group,ptr,g3;
	int i,ldtmp;
	int c1,c2;

	enemy_on_diag = FALSE;

	two_stone_wall = ld[s] >= 5;
	if(ld[s] == 4){
		i = fdir[s];
		for(ldtmp = ldiag[i]; i < ldtmp; ++i){
			g3 = board[s+diags[i]];
			if(g3 == NOGROUP)continue;
			if(grcolor[g3] == 1-c && !grthreatened[g3] &&
			   gralive[g3] != DEAD)
				++enemy_on_diag;
			}
		if(enemy_on_diag <= 1 && edge[s] > 1)
			for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
				c1 = grcolor[board[list[ptr]+dir2]];
				c2 = grcolor[board[list[ptr]-dir2]];
				if(c1 == c && c2 == NOCOLOR ||
				   c1 == NOCOLOR && c2 == c)
					two_stone_wall = TRUE;
				}
				
		}
	conn_with_peep = ld[s] == NEUTRALLD;
	two_lib_group = grlibs[g] == 2 || grlibs[g2] == 2;
	
	if(two_stone_wall)return(SOLID_CONNECT);

	if(!conn_with_peep){
		if(grlibs[g] > 2 && grlibs[g2] > 2)
			for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr])
				if(lnbf[list[ptr]][1-c] == 0 && 
				   (ld[list[ptr]+dir2] != NEUTRALLD &&
				    lnbn[list[ptr]+dir2] < 3 || 
				    ld[list[ptr]-dir2] != NEUTRALLD &&
				    lnbn[list[ptr]-dir2] < 3))
					return(AJI_CONNECT);
		if(edge[s] == 2 && 
		   grlibs[g] > 2 && grlibs[g2] > 2 && enemy_on_diag < 2)
			return(AJI_CONNECT);
		if(edge[s] > 1 && lnbf[s+dir][1-c] >= 2 && lnbf[s+dir][c] == 0 && 
		   gralive[lgr[s+dir]] != DEAD){
			if(can_def_peep(cn,g,g2,c,s,dir,dir2))
				return(AJI_CONNECT);
			else
				return(CAN_CONNECT);
			}
		if(edge[s] > 1 && lnbf[s-dir][1-c] >= 2 && lnbf[s-dir][c] == 0 && 
		   gralive[lgr[s-dir]] != DEAD){
			if(can_def_peep(cn,g,g2,c,s,-dir,dir2))
				return(AJI_CONNECT);
			else
				return(CAN_CONNECT);
			}
		if(edge[s] <= 4 &&
		   grlibs[g] > 3 && grlibs[g2] > 3 && enemy_on_diag < 2)
			return(AJI_CONNECT);
		if(!enemy_on_diag && !two_lib_group)
			return(AJI_CONNECT);
		}
	else if(edge[s] > 1 && lnbn[s] != 0 && can_def_peep(cn,g,g2,c,s,dir,dir2)){   
		return(AJI_CONNECT);
		}

	return(CAN_CONNECT);
	}


/* return TRUE if can stay connected if opponent pushes through cn at s from
 * dir is to enemy (or empty square if no enemy), dir2 is to friend 
 */

int can_def_peep(int cn,int g,int g2,int c,int s,int dir,int dir2){
	int i,ldtmp,sn,already_cut,cant_cut_above,cant_cut_below,can_push_through;
	i = fdir[s-dir+dir2];
	for(ldtmp = ldir[i]; i < ldtmp; ++i){
		sn = s-dir+dir2+nbr[i];
		if(addlist(NUMGROUPS+cn,&ldrflag[sn]))
			adflist(sn,&grldr[NUMGROUPS+cn]);
		}
	i = fdir[s-dir-dir2];
	for(ldtmp = ldir[i]; i < ldtmp; ++i){
		sn = s-dir-dir2+nbr[i];
		if(addlist(NUMGROUPS+cn,&ldrflag[sn]))
		adflist(sn,&grldr[NUMGROUPS+cn]);
		}
	can_push_through = grlibs[g] < 3 || grlibs[g2] < 3;
	if(!can_push_through){
		if(otherweakconn(g,g2))can_push_through = TRUE;
		if(otherweakconn(g2,g))can_push_through = TRUE;
		}
	already_cut = grcolor[board[s-dir+dir2]] == 1-c ||
		grcolor[board[s-dir-dir2]] == 1-c;
	cant_cut_above = ld[s-dir+dir2] > 3 || 
			(ld[s-dir+dir2] == 0 && grlibs[g] > 3 ) ||
			(edge[s-dir+dir2] == 2 && ld[s-dir+dir2] > 1 && grlibs[g] > 3);
	cant_cut_below = ld[s-dir-dir2] > 3 || 
		(ld[s-dir-dir2] == 0 && grlibs[g2] > 3) ||
			(edge[s-dir-dir2] == 2 && ld[s-dir-dir2] > 1 && grlibs[g2] > 3);
	if(!can_push_through && cant_cut_above && cant_cut_below &&
	   !already_cut)
		return(TRUE);
	return(FALSE);
	}

  
/* return AJI_CONECT if 2 point jump at s and s2 is unbreakable.
 * unbreakable 2 point jumps are along edge or from two stone wall
 * needs lots more work!
 */


int prot_two_point(int g,int g2,int s,int s2){
	int stone_g,stone_g2,tmp,along_edge,enough_libs,near_edge;
	int two_stone_wall,enemy_near,c,ptr,i,ldtmp;
	c = grcolor[g];
      	stone_g2 = s2*2-s;
      	stone_g = s*2-s2;
      	if(board[stone_g] != g){
         	tmp = g;
         	g = g2;
         	g2 = tmp;
         	}
      	if(board[stone_g] != g || board[stone_g2] != g2){
		return(CANT_CONNECT);
		}
	if(lnbf[s][1-c] + lnbf[s2][1-c] >= 3)return(CANT_CONNECT);
	if(lnbf[s][1-c] + lnbf[s2][1-c] == 2)return(twoenemytwopoint(s,s2,c));
      	along_edge = edge[s] == edge[s2];
	enemy_near = FALSE;
	for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr])
		if(lnbf[list[ptr]][1-c] != 0){
			enemy_near = TRUE;
			break;
			}
	if(!enemy_near)
		for(ptr = nblbp[s2]; ptr != EOL; ptr = links[ptr])
			if(lnbf[list[ptr]][1-c] != 0){
				enemy_near = TRUE;
				break;
				}
   	enough_libs = (grlibs[g] > 3 || grlibs[g] == 3 && edge[stone_g] == 1 ||
		       grlibs[g] == 3 && ld[s] > 4) && 
                 (grlibs[g2] > 3 || grlibs[g2] == 3 && edge[stone_g2] == 1 ||
		  grlibs[g2] == 3 && ld[s2] > 4);
   	near_edge = edge[s] <= 4;
   	if(enough_libs && near_edge && along_edge &&
      		ld[s] != NEUTRALLD && ld[s2] != NEUTRALLD)return(AJI_CONNECT);

   	two_stone_wall = (ld[s] > 3 || ld[s2] > 3) && ld[s] != NEUTRALLD &&
      		ld[s2] != NEUTRALLD;

   	if(enough_libs && two_stone_wall ||
	   enough_libs && !enemy_near){
		i = fdir[s];
		for(ldtmp = ldir[i]; i < ldtmp; ++i)
			if(grlibs[board[s+nbr[i]]] == 2)return(CAN_CONNECT);
		i = fdir[s2];
		for(ldtmp = ldir[i]; i < ldtmp; ++i)
			if(grlibs[board[s2+nbr[i]]] == 2)return(CAN_CONNECT);
		return(AJI_CONNECT);
		}
	return(CAN_CONNECT);
	}

/* two point jump with two enemy stones next to it */

int twoenemytwopoint(int s,int s2,int c){
	if(lnbf[s][1-c] == 2){
		if(lnbf[s2][c] > 1)return(CAN_CONNECT);
		}
	if(lnbf[s2][1-c] == 2){
		if(lnbf[s][c] > 1)return(CAN_CONNECT);
		}
	return(CANT_CONNECT);
	}

/* analyze a connection of two linkages, and no links */

int cntwolkgs(int g,int g2,int c,int cn){
	int s,s2,delta;
	if(grthreatened[g] || grthreatened[g2])return(CANT_CONNECT);
	s = list[cnllptr[cn]];
	s2 = list[links[cnllptr[cn]]];
	delta = s2-s;
	if(delta == 2 || delta == 2*boardsize){ /* 3 pt jump */
		if(board[s+(s-s2)/2] != g && board[s2+(s2-s)/2] != g)
			return(CANT_CONNECT);
		if(board[s+(s-s2)/2] != g2 && board[s2+(s2-s)/2] != g2)
			return(CANT_CONNECT);
		cntype[cn] = CN_THREEPOINTJUMP;
		return(threepointjump(g,g2,c,cn,s,s2));
		}
	else if(delta == boardsize + 3){
		cntype[cn] = CN_LARGEKNIGHT;
		return(largeknight(g,g2,c,cn,s,s2,1));
		}
	else if(delta == boardsize - 3){
		cntype[cn] = CN_LARGEKNIGHT;
		return(largeknight(g,g2,c,cn,s,s2,-1));
		}
	else if(delta == 3*boardsize + 1){
		cntype[cn] = CN_LARGEKNIGHT;
		return(largeknight(g,g2,c,cn,s,s2,boardsize));
		}
	else if(delta == 3*boardsize -1){
		cntype[cn] = CN_LARGEKNIGHT;
		return(largeknight(g,g2,c,cn,s,s2,boardsize));
		}
	else {
		cntype[cn] = CN_UNKNOWN;
		return(CANT_CONNECT);
		}
	}

/* evaluate a three point jump */

int threepointjump(int g,int g2,int c,int cn,int s,int s2){
	int s3,near_edge;
	s3 = (s2+s)/2;
	if((edge[s] > 2 || edge[s2] > 2) &&
	   (grlibs[g] < 4 || grlibs[g2] < 4 || lnbf[s][1-c] != 0 ||
	   lnbf[s2][1-c] != 0 || lnbf[s3][1-c] != 0))return(CANT_CONNECT);
	if(lnbf[s][1-c] > 1 || lnbf[s2][1-c] > 1 || lnbf[s3][1-c] > 1)
		return(CANT_CONNECT);
	if(edge[s] > 1 &&  edge[s2] > 1 && (grlibs[g] < 4 || grlibs[g2] < 4))
		return(CANT_CONNECT);
	near_edge = edge[s] <= 4 && edge[s2] <= 4;
	if(near_edge && (ld[s] > 2 || ld[s2] > 2) &&
	   lnbf[s][1-c] == 0 && lnbf[s2][1-c] == 0 && lnbf[s3][1-c] == 0)
		return(AJI_CONNECT);
	return(CAN_CONNECT);
	}

/* evaluate large knight connection between g and g2.  s and s2 are the linkage
 * points.  (s is upper).  dir is direction from s to s2 
 */

int largeknight(int g,int g2,int c,int cn,int s,int s2,int dir){
	int on_edge,sn,sn2;
	on_edge = edge[s] <= 4 && edge[s2] <= 4 && edge[s] != edge[s2];
	sn = s + dir + dir;
	sn2 = s2-dir-dir;
	if(edge[s] == 1 && edge[s2] == 2 ||
	   edge[s] == 2 && edge[s2] == 1){  /* monkey jump */
		if(grlibs[g] >= 3 && grlibs[g2] >= 3)
			return(AJI_CONNECT);
		else
			return(CAN_CONNECT);
		}
					       
	if(grlibs[g] < 4 || grlibs[g2] < 4 || 
	   lnbf[sn][1-c] != 0 || lnbf[sn2][1-c] != 0)
		return(CANT_CONNECT);
	if(on_edge || ld[s] > 3 || ld[s2] > 3 || lnbf[sn][c] > 1 ||
	   lnbf[sn2][c] > 1)return(AJI_CONNECT);
	return(CAN_CONNECT);
	}
  
/* analyze a connection of two links */

int linktypes[52];
    /* 1-2pt jump. 2-nights move. 0-neither.  types by distance */
 
int cntwolinks(int g,int g2,int c,int cn){
   int s,s2,sn,sn2,stone_g,stone_g2,delta,type,tmp,from_4_to_2;
   int two_point_jump,knights_move,enough_libs,can_push_cut;
   int near_edge,along_edge,prot_shape,one_side_threatened;
   one_side_threatened = 
	grthreatened[g] || grthreatened[g2];
   s = list[cnlkptr[cn]];
   s2 = list[links[cnlkptr[cn]]];
   delta = abs(s2-s);
   if(delta > 39)delta = 40;
   type = linktypes[delta];

   two_point_jump = type == 1;
   if(two_point_jump){
	   if(board[s+s-s2] != g && board[s2+s2-s] != g)
		   two_point_jump = FALSE;
	   if(board[s+s-s2] != g2 && board[s2+s2-s] != g2)
		   two_point_jump = FALSE;
	   }
   knights_move = type == 2;
   if(!two_point_jump && !knights_move)return(CAN_CONNECT);
   if(two_point_jump){
	cntype[cn] = CN_TWOPOINTJUMP;
	if(one_side_threatened)return(CANT_CONNECT);
	return(prot_two_point(g,g2,s,s2));
	}

	/* now check knights move to see if it is unbreakable 
	 *
	 *	stone_g2 O  sn2 s
	 *	         s2 sn  O stone_g
	 */


   cntype[cn] = CN_KNIGHTSMOVE;
   if(one_side_threatened)return(CAN_CONNECT);
   sn = s;
   sn2 = s2;
   delta = xval[s2]-xval[s];
   if(delta == 2 || delta == -2){
         stone_g = s2 - delta;
         stone_g2 = s + delta;
         sn = s2 - delta/2;
         sn2 = s + delta/2;
         }
   else {
         stone_g = s2 - boardsize*2;
         stone_g2 = s + boardsize*2;
         sn = s2 - boardsize;
         sn2 = s + boardsize;
         }
   if(board[stone_g] != g){
         tmp = g;
         g = g2;
         g2 = tmp;
         }

   enough_libs = (grlibs[g] > 3 || grlibs[g] == 3 && edge[stone_g] == 1) && 
                 (grlibs[g2] > 3 || grlibs[g2] == 3 && edge[stone_g2] == 1);

   prot_shape = (ld[s] > 3 && ld[sn2] != NEUTRALLD) || (ld[s2] > 3 &&
		ld[sn] != NEUTRALLD);


   if(enough_libs && prot_shape)return(AJI_CONNECT);

   along_edge = abs(edge[s] - edge[s2]) < 2;

   near_edge = edge[s] <= 4 && edge[s2] <= 4;

   from_4_to_2 = edge[s2] == 4 && edge[s] == 2 ||
		edge[s2] == 2 && edge[s] == 4;

   can_push_cut = FALSE;
   if(ld[s] == NEUTRALLD && ld[sn] == NEUTRALLD){
	can_push_cut = checkpush(sn,sn2,s,cn,c);
	}
   if(ld[s2] == NEUTRALLD && ld[sn2] == NEUTRALLD){
	can_push_cut = checkpush(sn2,sn,s2,cn,c);
	}
   if(enough_libs && near_edge && along_edge && !can_push_cut)return(AJI_CONNECT);
   if(enough_libs && from_4_to_2 && !can_push_cut)return(AJI_CONNECT);
   return(CAN_CONNECT);
   }


/* does the sequence s1,s2,s3 push through the nights move.
 * after the sequence can the groups at s1 or s3 be captured.
 * return TRUE if they can't be captured (if push through and cut works)
 */

int checkpush(int s1,int s2,int s3,int cn,int c){
	int ldrno,mlibs,g,flag;
/*	if(noladder)return(TRUE); */
	ldrno = cn + NUMGROUPS;
	mvs[msptr] = s1;
	mvcolor[msptr] = 1-c;
	flag = lupdate(msptr);
	upldrflags(msptr,ldrno);
	if(flag){
		++msptr;
		mvs[msptr] = s2;
		mvcolor[msptr] = c;
		flag = lupdate(msptr);
		upldrflags(msptr,ldrno);
		if(flag){
			++msptr;
			mvs[msptr] = s3;
			mvcolor[msptr] = 1-c;
			flag = lupdate(msptr);
			upldrflags(msptr,ldrno);
			if(flag){
				++msptr;
				g = board[s1];
				mlibs = 2;
				if(edge[s1] < 4)mlibs = 3;
				if(grlibs[g] <= mlibs)
					flag = !iscaptured(g,9,playlevel,mlibs,c,ldrno);
				if(flag){
					g = board[s3];
					mlibs = 2;
					if(edge[s1] < 4)mlibs = 3;
					if(grlibs[g] <= mlibs)
						flag = !iscaptured(g,9,playlevel,mlibs,c,ldrno);
					}
				--msptr;
				}
			ldndate(msptr);
			--msptr;
			}
		ldndate(msptr);
		--msptr;
		}
	ldndate(msptr);
	return(flag);
	}

/* figure out protection value for diagonal connection */

int protdiag(int cn,int s1,int s2,int c){
	int ptr,s,sn,so,i,g1,g2,ldtmp,dir;
	if(ld[s1] != NEUTRALLD && ld[s2] != NEUTRALLD)return(SOLID_CONNECT);
	g1 = cngr1[cn];
	g2 = cngr2[cn];
	for(ptr = cnptr[cn]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(ld[s] != NEUTRALLD)continue;
		if(lnbf[s][c] != 3)continue;
		if(links[cnbrd[s]] == EOL)continue;
		i = fdir[s];
		for(ldtmp = ldir[i]; i != ldtmp; ++i){
			sn = s + nbr[i];
			if(grcolor[board[sn]] == 1-c){
				dir = nbr[i];
				break;
				}
			}
		if(grthreatened[board[sn]])continue;
		so = s1;
		if(s1 == s)so = s2;  /* other diagonal connection */

		/* is other diagonal defended */
		if(lnbf[so][1-c] == 0){
			if(lnbn[so] <= 1)continue;
			if(edge[so] == 2 && edge[s] == 3)continue;
			if(ld[so] >= 6)continue;
			}
		

		i = fdir[s];
		for(ldtmp = ldir[i]; i != ldtmp; ++i){
			sn = s + nbr[i];
			if(grcolor[board[sn]] == c && board[sn] != g1 &&
			   board[sn] != g2 && grlibs[board[sn]] == 2)
				return(SHARED_CONNECT);
			}
		so = s - dir + s - dir - so;
		if(addlist(NUMGROUPS+cn,&ldrflag[so]))
			adflist(so,&grldr[NUMGROUPS+cn]);
		if(ld[so] >= 6)continue;
		if(grcolor[board[so]] == 1-c)continue;  /* already cut */
		return(SHARED_CONNECT);
		}
	return(AJI_CONNECT);
	}

void fixcnprot(void){
   int ptr,cn,c,g,g2,s1,s2,diff,s;
   int oldprot;
   int one_side_dead,no_two_libs,one_side_threatened;
   for(ptr = cnchgd; ptr != EOL; ptr = links[ptr]){

      cn = list[ptr];
      if(grldr[NUMGROUPS+cn] != EOL)kill_ldrflags(NUMGROUPS+cn);
      oldprot = cnprot[cn];
      cnprot[cn] = CANT_CONNECT;
      cntype[cn] = CN_UNKNOWN;
      g = cngr2[cn];
      g2 = cngr1[cn];
      c = grcolor[g];

      if(cnlknum[cn] == 0 && cncnum[cn] == 0 && cnllnum[cn] == 0){

         /* addlist(cn,&cnfreelist); an addlist here can put a connection
	           in the freelist twice! */
	 continue;
         }
      one_side_dead = 
           gralive[g] == DEAD || gralive[g2] == DEAD;
      one_side_threatened = 
	   grthreatened[g] || grthreatened[g2];
      no_two_libs = grlibs[g] > 2 && grlibs[g2] > 2;
      if(one_side_dead)
         cntype[cn] = CN_ONESIDEDEAD;
      else if(cncnum[cn] > 1){
	 if(cncnum[cn] == 2){
		s1 = list[cnptr[cn]];
		s2 = list[links[cnptr[cn]]];
		diff = s2-s1;
		if(diff == 1 || diff == boardsize){
			cntype[cn] = CN_BAMBOOJOINT;
			cnprot[cn] = SOLID_CONNECT;
			}
		else if(diff == boardsize-1 || diff == boardsize+1){
			cntype[cn] = CN_DIAGONAL;
			cnprot[cn] = protdiag(cn,s1,s2,c);
			}
		else {
			cntype[cn] = CN_MULTIPLE;
			cnprot[cn] = SOLID_CONNECT;
			}
		}
	 else {
         	cntype[cn] = CN_MULTIPLE;
		cnprot[cn] = SOLID_CONNECT;
		}
	 if(one_side_threatened)cnprot[cn] = CAN_CONNECT;
         }
      else if(cncnum[cn] == 1)   /* single connection */
         cnprot[cn] = cnoneconn(g,g2,c,cn);
      else if(no_two_libs && cnlknum[cn] > 2){
	      cntype[cn] = CN_MULTIPLE;
	      if(one_side_threatened)
		      cnprot[cn] = CAN_CONNECT;
	      else
		      cnprot[cn] = AJI_CONNECT;
	      }
      else if(cnlknum[cn] == 2)
                              /* check linkages for unbreakable conns */
         cnprot[cn] = cntwolinks(g,g2,c,cn);
      else if(cnlknum[cn] == 1){
	      cntype[cn] = CN_HALFKNIGHT;
	      cnprot[cn] = canconnlink(cn,&s);
	      }
      else if(cnllnum[cn] > 2){
	      cntype[cn] = CN_MULTIPLE;
	      cnprot[cn] = CAN_CONNECT;
	      }
      else if(cnllnum[cn] == 2)
	      cnprot[cn] = cntwolkgs(g,g2,c,cn);
      else if(cnllnum[cn] == 1){
	      cntype[cn] = CN_HALFLARGEKNIGHT;
	      if(canconnlkg(cn,&s,&s2))
		      cnprot[cn] = CAN_CONNECT;
	      else
		      cnprot[cn] = CANT_CONNECT;
	      }



      if(cnprot[cn] <= SHARED_CONNECT && oldprot >= AJI_CONNECT){
            if(grlv[g]){
               addlist(g,&splitlist);
               }
            if(grlv[g2]){
               addlist(g2,&splitlist);
               }
            }
      else if(cnprot[cn] >= AJI_CONNECT && oldprot <= SHARED_CONNECT){

            combinearmy(grarmy[g],grarmy[g2]);
            }
      }
   killist(&cnchgd);
   }
  

void combinearmy(int a1,int a2){  /* combine two armies into one */
                                 /* a1 goes away, a2 gets bigger */
   int g,ptr;
   if(a1 == a2){
	return;
	}
   for(ptr = armygroups[a1]; ptr != EOL; ptr = links[ptr]){
      g = list[ptr];
      grarmy[g] = a2;
      }
   for(ptr = armydeadgroups[a1]; ptr != EOL; ptr = links[ptr])
      grdeadarmy[list[ptr]] = a2;
   mrglist(armygroups[a1],&armygroups[a2]);
   mrglist(armydeadgroups[a1],&armydeadgroups[a2]);
   armysize[a2] += armysize[a1];
   make_army_free(a1);
   }


/* splitarmy is called when an army is no longer in one piece.
 * It must be called after bdead completes since all completely
 * dead groups must be already known.  There are two reasons to call
 * splitarmy.  If a protected connection becomes breakable, or
 * if a dead group becomes not dead.  Note that this includes the cases
 * where a connection or a group goes away completely.
 * splitarmy is also called when a dead group becomes not a neighbor
 * any more or a new dead group becomes a neighbor.
 */


void splitarmy(int a){   /* split one army into its pieces */
   int army,ptr,g,connptr,ctptr,dgrptr,ctp,ctpt2,g2,ctgr,cn;
   int gptr,g3,ptr2;
   for(gptr = armygroups[a]; gptr != EOL; gptr = links[gptr])
   if(grarmy[list[gptr]] == a){
      g = list[gptr];
      army = gtflist(&armyfreelist);
      if(army == ERROR){
      			outerror(0,"Out of armies, can't continue");
      			turnoffcplay();
      			continue;
      			}

      connptr = EOL;
      ctptr = EOL;
      dgrptr = EOL;
      addlist(g,&connptr);
      grarmy[g] = army;
      armysize[army] = grsize[g];
      if(gralive[g] != DEAD && !grthreatened[g]){
        adflist(g,&ctptr);
        do{
  
         ctpt2 = EOL;
  
         for(ctp = ctptr; ctp != EOL; ctp = links[ctp]){
            ctgr = list[ctp]; 

  
                                   /* count groups with unbreakable conns*/ 
            for(ptr = grcnp[ctgr]; ptr != EOL; ptr = links[ptr]){
               cn = list[ptr];

               if(PROTCONN(cn)){
                  g2 = cngr1[cn]; 
                  if(g2 == ctgr)g2 = cngr2[cn]; 

		  if(grarmy[g2] != army){
		     grarmy[g2] = army;
		     armysize[army] += grsize[g2];
                     addlist(g2,&connptr);
                     adflist(g2,&ctpt2); 
                     } 
                  } 
               }
  
            for(ptr = grnbp[ctgr]; ptr != EOL; ptr = links[ptr]){
                          /* dead groups */
               g3 = list[ptr];
               if(gralive[g3] == DEAD){
		  if(grdeadarmy[g3] != army){
                     addlist(g3,&dgrptr);
		     grdeadarmy[g3] = army;
                     for(ptr2 = grnbp[g3]; ptr2 != EOL; ptr2 = links[ptr2]){
                        g2 = list[ptr2];
                        if(g2 != g && gralive[g2] != DEAD && !grthreatened[g2]){
			   if(grarmy[g2] == a){
                              addlist(g2,&connptr);
			      grarmy[g2] = army;
			      armysize[army] += grsize[g2];
                              adflist(g2,&ctpt2);
                              }
                           }
                        }

                     }
                  }
               }

            }
  
         killist(&ctptr);
         ctptr = ctpt2;
         }while(ctpt2 != EOL);
	}
      armygroups[army] = connptr;
      armydeadgroups[army] = dgrptr;
      }
   make_army_free(a);
   }

  
/* cntplyhere is called with a square, a color, and a ladder number.
 * it returns YES if a stone played on that square, of the opposite color,
 * is captured in a short ladder.  It is used to determine if cuts
 * or eye stealing tesuji are possible.
 * returns NO if the stone can't be captured.
 * returns MAYBE if can't tell (ko or ladder is inconclusive)
 */


int cntplyhere(int msqr,int mclr,int ldrno){
   int i,ldtmp,g,flag,sn,can_capture=FALSE;
   int s,numescape=0;
  

   if(addlist(ldrno,&ldrflag[msqr]))
      adflist(msqr,&grldr[ldrno]);

   if(lnbn[msqr] > quicklibs){  /* stone will get 3 liberties, not a ladder */
      return(NO);
      }

   i = fdir[msqr];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      sn = msqr + nbr[i];
      if(ld[sn] != 0){	/* empty square */
	 if(lnbn[sn] > quicklibs+1)numescape++;
         continue;
         }
      g = lgr[sn];
      if(grcolor[g] == mclr){
	      if(grlibs[g] == 1)can_capture = TRUE;
	      continue;
	      }
      if(gralive[g] == DEAD){
         return(YES);     /* nets */
         }
      if(grlibs[g] + lnbn[msqr] > quicklibs+1){
	      s = mvs[grpieces[g]];
	      if(addlist(ldrno,&ldrflag[s]))
		      adflist(s,&grldr[ldrno]);
	      return(NO);  /* new stone part of bigger group with enough libs */
	      }
      }
   if(lnbf[msqr][1-mclr] == 0 && lnbn[msqr] < 2 && !can_capture)return(YES);
   if(numescape > 1)return(NO);
   if(msqr == kosquare)return(MAYBE);  /* can't play in ko square */
   if(can_capture)return(NO);
   if(noladder)return(NO);
   flag = FALSE;
   mvs[msptr] = msqr;
   mvcolor[msptr] = 1-mclr;
   flag = lupdate(msptr);  /* put down stone */
   upldrflags(msptr,ldrno);

   ++msptr;
   if(flag){
      g = board[msqr];
      if(grlibs[g] == 1 &&
        (grsize[g] != 1 || grpieces[g] != msptr-1 ||
         mvcapt[msptr-1] == EOL || grsize[list[mvcapt[msptr-1]]] != 1)){   
         s = list[grlbp[g]];
         if(addlist(ldrno,&ldrflag[s]))
            adflist(s,&grldr[ldrno]);
         flag = FALSE;     /* can capture */
         }
      if((grlibs[g] == 2 || grlibs[g] == 3 && edge[msqr] < 4)
         && playlevel > 4){

         flag = !iscaptured(g,11,playlevel,quicklibs,mclr,ldrno);
         }
      }
   --msptr;
   ldndate(msptr);   /* take stone away */

   return(!flag);
   }
