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
  
  
extern int pfac[];
extern int cfac[];

/* add liberties due to stone going away
 * s is square where stone was taken away.
 * gb is group number of group being destroyed
 * c is color of stone removed
 */
  
void adplib(int s,int gb,int c){
   int sn,i;
   int g,ldtmp;
   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i ){
      sn = s + nbr[i];
      --lnbf[sn][c];
      ++lnbn[sn];
      addlist(s,&nblbp[sn]);
      g = board[sn];
      if(g == NOGROUP)continue;
      if(g == gb)continue;
      if(addlist(s,&grlbp[g])){
        ++grlibs[g];
        } 
      } 
   }
  
 
void deplib(int s,int gb){   /* subtract liberties due to stone resurrecting */
   int sn,i;
   int g,ldtmp;
   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i ){
      sn = s + nbr[i];
      ++lnbf[sn][grcolor[gb]];
      --lnbn[sn];
      dellist(s,&nblbp[sn]);
      g = board[sn];
      if(g == NOGROUP)continue;
      if(g == gb)continue;
      if(dellist(s,&grlbp[g])){
         --grlibs[g];
         }
      addlist(g,&grnbp[gb]);
      addlist(gb,&grnbp[g]);
      }
   }
  
void adlibs(int s,int g) {    /* add liberties due to new stone in group */ 
  int sn,i,g2,ldtmp;
  i = fdir[s];
  for(ldtmp = ldir[i]; i < ldtmp; ++i){ 
     sn = s + nbr[i]; 
     g2 = board[sn];
     if(g2 == NOGROUP){ 
        if(addlist(sn,&grlbp[g]))++grlibs[g]; 
        } 
     else if(grcolor[g2] != grcolor[g]){
        addlist(g2,&grnbp[g]);
        addlist(g,&grnbp[g2]);
        } 
     }
   }
  
  
void delibs(int s,int g){ /* delete liberties,neighbors due to stone going away */ 
   int sn,i,j,sn2,lflag,nflag,g1,g2,ptr,k,sn3,ldtmp,ldtm2;
   i = fdir[s]; 
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      sn = s + nbr[i];
      g1 = board[sn]; 
      nflag = FALSE;
      if(g1 != NOGROUP){
        if(grcolor[g1] == grcolor[g])continue;
        if(grsize[g1] > grsize[g]){ 
           g2 = g1; 
           g1 = g;
           }
        else
           g2 = g;
                    /* search the smallest group */ 
        ptr = grpieces[g1]; 
        while(ptr != -1){
           sn2 = mvs[ptr];
           if(edge[sn2] < 2){ 
            k = fdir[sn2];
            for(ldtm2 = ldir[k]; k < ldtm2; ++k){
              sn3 = sn2 + nbr[k];
              if(board[sn3] == g2){
                 nflag = TRUE;
                 break;
                 }
              }
            }
           else
              if(board[sn2+1] == g2 || board[sn2-1] == g2 ||
                 board[sn2+boardsize]
                 == g2 || board[sn2-boardsize] == g2)nflag = TRUE;
           if(nflag)break;
           ptr = mvnext[ptr];
           }
        if(!nflag){
           dellist(g1,&grnbp[g2]);
           dellist(g2,&grnbp[g1]);
           }
        continue;
        }
      lflag = TRUE;
      if(edge[sn] < 2){
       j = fdir[sn];
       for(ldtm2 = ldir[j]; j < ldtm2; ++j){
         sn2 = sn + nbr[j];
         if(board[sn2] == g){
            lflag = FALSE;
            break;
            }
         }
       }
      else
         if(board[sn+1] == g || board[sn-1] == g ||
          board[sn+boardsize] == g || board[sn-boardsize] == g)lflag = FALSE;
      if(lflag){
         --grlibs[g];
         dellist(sn,&grlbp[g]); 
         }
      } 
   }
  
  
  

void resurrect(int g,int c) {    /* bring back group g */
   int lptr,s;
   grlv[g] = TRUE;
   lptr = grpieces[g];
   while(lptr != -1){
       --numpris[c];
       s = mvs[lptr]; 
       board[s] = g;
  
       upxy(s); 
  
       pcls[pclsnext++] = s;

       uscan(s);
       deplib(s,g); 	/* fix liberty and neighbor lists */
       brkconns(s);    /* break connections due to resurrection*/
       lptr = mvnext[lptr]; 
       }
   pscr += pfac[gralive[g]&31] * grsize[g] * cfac[grcolor[g]];
   newgrouparmy(g);
   }

void newgrouparmy(int g){
   int army,g2,ptr;
   army = -1;
   for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
      g2 = list[ptr];
      if((gralive[g2] & 31) == DEAD && grdeadarmy[g2] != NOARMY)
	 army = grdeadarmy[g2];
      }
   if(army == -1){
      army = gtflist(&armyfreelist);
      armysize[army] = grsize[g];
      }
   else
      armysize[army] += grsize[g];
   grarmy[g] = army;

   addlist(g,&armygroups[army]);
   }
  
  
void dndate(int dnptr) {
   int s,sn2,lptr,c,g,g2,x,y,ptr,lptr2,army;
   kosquare = kosave[dnptr];
   s = mvs[dnptr];
   if(s == PASS)return;
   c = mvcolor[dnptr];
   if(edge[s] <= 4){
      x = xval[s];
      y = yval[s];
      if(xmin > x-3)xmin = x-3;
      if(xmax < x+3)xmax = x+3;
      if(ymin > y-3)ymin = y-3;
      if(ymax < y+3)ymax = y+3;
      }
   g = board[s];
  
   for(ptr = mvcapt[dnptr]; ptr != EOL; ptr = links[ptr]){ 
                         /* bring back killed groups */ 
         g2 = list[ptr];
         resurrect(g2,c);  /* resurrect enemy group */
         }
   for(ptr = mvconn[dnptr]; ptr != EOL; ptr = links[ptr]){
                         /* split combined groups */
         g2 = list[ptr];
         split(g2,g);
	 if(grlibs[g2] == 2){
		 sn2 = list[grlbp[g2]];
		 if(sn2 == s)sn2 = list[links[grlbp[g2]]];
		 if(edge[sn2] <= 4){
			 x = xval[sn2];
			 y = yval[sn2];
			 if(xmin > x-3)xmin = x-3;
			 if(xmax < x+3)xmax = x+3;
			 if(ymin > y-3)ymin = y-3;
			 if(ymax < y+3)ymax = y+3;
			 }
		 }
         }
   if(grlibs[g] == 1){
	   sn2 = list[grlbp[g]];
	   if(edge[sn2] <= 4){
		   x = xval[sn2];
		   y = yval[sn2];
		   if(xmin > x-3)xmin = x-3;
		   if(xmax < x+3)xmax = x+3;
		   if(ymin > y-3)ymin = y-3;
		   if(ymax < y+3)ymax = y+3;
		   }
	   }
   dscan(s);
   dncons(s,g); 
   dnxy(s,NOCOLOR); 
   board[s] = NOGROUP;
   pscr -= pfac[gralive[g]&31] * cfac[grcolor[g]];
   --grsize[g]; 
   --armysize[grarmy[g]];
  
   pcls[pclsnext++] = s;
   if(grsize[g] == 0){  /* only one piece in group */ 

      if(grdeadarmy[g] != NOARMY){

         dellist(g,&armydeadgroups[grdeadarmy[g]]);
         grdeadarmy[g] = NOARMY;
         }
      army = grarmy[g];
      dellist(g,&armygroups[army]);
      if(armygroups[army] == EOL){
	 make_army_free(army);
         }
      killist(&grlbp[g]);     /* return liberties to free list */
      grlibs[g] = 0;
      grlv[g] = FALSE;
      if((gralive[g]&31) == DEAD && grnbp[g] != EOL)
         addlist(mvs[grpieces[list[grnbp[g]]]],&charmy);
      lptr = grnbp[g];
      while(lptr != EOL){
         dellist(g,&grnbp[list[lptr]]);
         lptr = links[lptr];
         }
      killist(&grnbp[g]); /* return neighbors to free list */
  
      for(lptr = grldr[g]; lptr != EOL; lptr = links[lptr])
         dellist(g,&ldrflag[list[lptr]]);
      killist(&grldr[g]); /* return ladder flags to free list */
      --maxgr;      /* delete group */
      }
   else {                  /* take piece away from group */
      lptr = grpieces[g];
      while(mvnext[lptr] != -1){
         lptr2 = mvnext[lptr];
         if(mvs[lptr2] == s){
            mvnext[lptr] = -1;
            break;
            } 
         lptr = mvnext[lptr]; 
         }
      delibs(s,g);
      } 
   adplib(s,NOGROUP,c); 
   if(ld[s] != NOLD)adcons(s,NOGROUP);
   }


void make_army_free(int army){
	int ptr;
   	int i;
   	for(i = 0; i < NUMRUN; ++i)killist(&armyrun[army][i]);

	killist(&armygroups[army]);
        for(ptr = armydeadgroups[army]; ptr != EOL; ptr = links[ptr])
		if(grdeadarmy[list[ptr]] == army)
	        	grdeadarmy[list[ptr]] = NOARMY;
        killist(&armydeadgroups[army]);
	killist(&armylbp[army]);
	if(armynbp[army] != EOL)killist(&armynbp[army]);
        killist(&armyvitalpoints[army]);
        killist(&armyeyerecs[army]);
	armylibs[army] = 0;
        adflist(army,&armyfreelist);
	killist(&armypot[army]);
	}

void split(int i,int j){   /* bring i back to life and split it from j */ 
   int lptr,army,ptr;
   grlibs[j] = 0; 
   grlv[i] = TRUE;
   killist(&grlbp[j]); 
   if((gralive[j]&31) == DEAD)
      for(ptr = grnbp[j]; ptr != EOL; ptr = links[ptr]){
         addlist(mvs[grpieces[list[ptr]]],&charmy);
         }
   lptr = grnbp[j]; 
   while(lptr != EOL){
      dellist(j,&grnbp[list[lptr]]);
      lptr = links[lptr];
      } 
   killist(&grnbp[j]); 
   grsize[j] -= grsize[i];
   army = grarmy[j];
   grarmy[i] = army;
   addlist(i,&armygroups[army]); 
   for(lptr = grpieces[i]; lptr != -1; lptr = mvnext[lptr]){
      adlibs(mvs[lptr],i);
      board[mvs[lptr]] = i; 
      cscan(mvs[lptr],j); 
      dncons(mvs[lptr],j);  
      chkcon(mvs[lptr],i);
      rstrlks(i,j,mvs[lptr]);
      } 
   lptr = grpieces[j];
   while(mvnext[lptr] != grpieces[i]) 
      lptr = mvnext[lptr];
   mvnext[lptr] = -1;
   lptr = grpieces[j];
   while(lptr != -1){
      adlibs(mvs[lptr],j);
      lptr = mvnext[lptr];
      } 
   gralive[i] = gralive[j]; 
   }
  
/* make a move for real.  do all the update stuff, and also 
 * update the shpbrd structure and the josekis structures
 */

int real_update(int upptr,int nosuicide){
	int t,s;
	t = update(upptr,nosuicide);
	if(t){
		s = mvs[upptr];
		if(s == PASS)return(t);
	      	jupdate(s,mvcolor[upptr]);
	      	finds(upptr);
		}
	return(t);
	}  
	
/* find shapes after makeing or unmaking this move */

void finds(int mptr){
          int s,fsqr,lsqr,ptr,ptr2;
          s = mvs[mptr];
          fsqr = lsqr = s;
          for(ptr = mvcapt[mptr]; ptr != EOL; ptr = links[ptr])
                  for(ptr2 = grpieces[list[ptr]]; ptr2 != -1; ptr2 = mvnext[ptr2]){
                          s = mvs[ptr2];
                          if(xval[s] < xval[fsqr])
                                  fsqr = yval[fsqr]*boardsize + xval[s];
                          if(yval[s] < yval[fsqr])
                                  fsqr = yval[s]*boardsize + xval[fsqr];
                          if(xval[s] > xval[lsqr])
                                  lsqr = yval[lsqr]*boardsize + xval[s];
                          if(yval[s] > yval[lsqr])
                                  lsqr = yval[s]*boardsize + xval[lsqr];
                          }
	findshapes(fsqr,lsqr);
        }

/* update the data structures for a move made.
 * upptr is the index of the move to be made.
 * if nosuicide is TRUE, suicide is not allowed
 * update returns TRUE if the move is legal, FALSE otherwise
 */
  
  
int update(int upptr,int nosuicide){
   int i,s,c,sn,sn2,gflag,lptr;
   int g,gnew;
   int x,y,ldtmp,ptr;
   kosave[upptr] = kosquare;
   if(kosquare != NOSQUARE)
	gralive[lgr[kosquare]] |= 32;
   kosquare = NOSQUARE;
   s = mvs[upptr];
   c = mvcolor[upptr];
   killist(&mvconn[upptr]); 
   killist(&mvcapt[upptr]);
   atariflag = FALSE;
   if(s == PASS)return(TRUE);	/* pass always legal */
   if(edge[s] <= 4){
      x = xval[s];
      y = yval[s];
      if(xmin > x-3)xmin = x-3;
      if(xmax < x+3)xmax = x+3;
      if(ymin > y-3)ymin = y-3;
      if(ymax < y+3)ymax = y+3;
      }
   pcls[pclsnext++] = s;

   gflag = FALSE;
   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){   /* look at neighbors */ 
      sn = s + nbr[i];
      g = board[sn];
      ++lnbf[sn][c];
      --lnbn[sn]; 
      dellist(s,&nblbp[sn]);
      if(grcolor[g] == 1-c){      /* unfriendly neighbor */
         if(dellist(s,&grlbp[g])){
            --grlibs[g]; 
            if(grlibs[g] == 1){
               atariflag = TRUE; 
               } 
            else if(grlibs[g] == 0){  /* killed enemy group */ 
               adflist(g,&mvcapt[upptr]);
               } 
            }
         }
      } 
   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){ 
      sn = s + nbr[i];
      g = board[sn];
      if(grcolor[g] == c){   /* friendly neighbor */
	      if(grlibs[g] == 2){
		      sn2 = list[grlbp[g]];
		      if(sn2 == s)sn2 = list[links[grlbp[g]]];
		      if(edge[sn2] <= 4){
			      x = xval[sn2];
			      y = yval[sn2];
			      if(xmin > x-3)xmin = x-3;
			      if(xmax < x+3)xmax = x+3;
			      if(ymin > y-3)ymin = y-3;
			      if(ymax < y+3)ymax = y+3;
			      }
		      }
         if(!gflag){
            gnew = g; 
            gflag = TRUE; 
            adlibs(s,gnew); 
            board[s] = gnew;
            upxy(s);
            ++grsize[gnew]; 
            ++armysize[grarmy[gnew]];
            pscr += pfac[gralive[gnew]&31] * cfac[grcolor[gnew]]; 
            uscan(s); 
            --grlibs[gnew]; 
            chkcon(s,gnew); 
            dellist(s,&grlbp[gnew]);
            lptr = grpieces[gnew];
            while(mvnext[lptr] != -1)
               lptr = mvnext[lptr]; 
            mvnext[upptr] = -1;
            mvnext[lptr] = upptr; 
            } 
         else combine(gnew,g,upptr);
         }
      } 
  
  
    brkconns(s);  

    if(!gflag)   /* create new group */
	makenewgroup(s,c,upptr);

    for(ptr = mvcapt[upptr]; ptr != EOL; ptr = links[ptr])
       kilgrp(list[ptr],c);  
    if(grlibs[board[s]] == 0)
	if(nosuicide)return(FALSE);  /* illegal suicide */
	else {
		addlist(board[s],&mvcapt[upptr]);
		kilgrp(board[s],1-c);
		}
    return(TRUE);
    } 



/* start a new group at s with color c */

int makenewgroup(int s,int c,int upptr){
	int j,i,ldtmp,sn,army,ptr;

         j = maxgr++; 
         board[s] = j;
         grsize[j] = 1; 
         gralive[j] = 1; 
         grthreatened[j] = FALSE;
         grlv[j] = TRUE;
         grcolor[j] = c;
         grlibs[j] = 0;
         army = gtflist(&armyfreelist);

         grarmy[j] = army;
         armysize[army] = 1;
         adflist(j,&armygroups[army]);
         upxy(s); 
         i = fdir[s]; 
         for(ldtmp = ldir[i]; i < ldtmp; ++i){
            sn = s + nbr[i];
            if(board[sn] == NOGROUP){
		addlist(sn,&grlbp[j]);
		++grlibs[j];
		}
            else{
               addlist(board[sn],&grnbp[j]);
               addlist(j,&grnbp[board[sn]]);
               }
            }
         if(grlibs[j] == 0 && mvcapt[upptr] != EOL &&
            grsize[list[mvcapt[upptr]]] == 1 &&
            links[mvcapt[upptr]] == EOL){
            kosquare = mvs[grpieces[list[mvcapt[upptr]]]];
            }
         uscan(s);
         mvnext[upptr] = -1;
         grpieces[maxgr-1] = upptr;
         chkcon(s,board[s]);
         for(ptr = grnbp[j]; ptr != EOL; ptr = links[ptr])
            if((gralive[list[ptr]]&31) == DEAD){
	       if(grdeadarmy[list[ptr]] != NOARMY)
                     combinearmy(grarmy[j],grdeadarmy[list[ptr]]);
              }
	return(TRUE);
         }
  
  
/* remove group g from board since it is captured
 */

  
void kilgrp(int g,int c){
   int lptr,ptr2,s,army;

   pscr -= pfac[gralive[g]&31] * grsize[g] * cfac[grcolor[g]]; 
   if(grdeadarmy[g] != NOARMY){
      dellist(g,&armydeadgroups[grdeadarmy[g]]);
      grdeadarmy[g] = NOARMY;
      }
   gralive[g] = 1; 
   grlv[g] = FALSE;
   addlist(mvs[grpieces[list[grnbp[g]]]],&charmy);
 
   lptr = grnbp[g];
   while(lptr != EOL){
      dellist(g,&grnbp[list[lptr]]);
      lptr = links[lptr];
      }
   killist(&grnbp[g]);

   army = grarmy[g];
   dellist(g,&armygroups[army]);
   grarmy[g] = NOARMY;
   armysize[army] -= grsize[g];
   if(armygroups[army] == EOL){
      make_army_free(army);
      }      

   ptr2 = grldr[g];
   while(ptr2 != EOL){
      dellist(g,&ldrflag[list[ptr2]]);
      ptr2 = links[ptr2];
      }
   killist(&grldr[g]);

   lptr = grpieces[g];
   while(lptr != -1){
      s = mvs[lptr];
      dscan(s);
      dnxy(s,1-c);	/* dont add links to this color */
      board[s] = NOGROUP;
      pcls[pclsnext++] = s;
      adplib(s,g,1-c);
      ++numpris[c];
      adcons(mvs[lptr],g);
      lptr = mvnext[lptr];
      }
  }
  
  
/* combine group j into group i */  
  
void combine(int i,int j,int upptr){
  int lptr,ptr,ptr2,army,g2,sn,x,y;
  if(i == j)return;
  if(grthreatened[j]){ /* eliminating threatened group*/
	for(ptr = grnbp[j]; ptr != EOL; ptr = links[ptr])
		for(ptr2 = grcnp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2])
			if(cntype[list[ptr2]] == CN_THREAT)
				addlist(list[ptr2],&cnchgd);
     }
  if((gralive[j] & 31) == DEAD){         /* eliminating dead group */
     for(lptr = grpieces[j]; lptr != -1; lptr = mvnext[lptr]){
        sn = mvs[lptr];
        addlist(sn,&eyelist);
        x = xval[sn];
        y = yval[sn];
        if(x-4 < xmin)xmin = x-4;
        if(x+4 > xmax)xmax = x+4;
        if(y-4 < ymin)ymin = y-4;
        if(y+4 > ymax)ymax = y+4;
        }
     if(grnbp[j] != EOL){
        addlist(mvs[grpieces[list[grnbp[j]]]],&charmy);
         }
     }
  pscr = pscr + (pfac[gralive[i]&31] - pfac[gralive[j]&31])*grsize[j] *
         cfac[grcolor[j]];
  if(grdeadarmy[j] != NOARMY){
     dellist(j,&armydeadgroups[grdeadarmy[j]]);
     grdeadarmy[j] = NOARMY;
     }
  grlibs[j] = 0;
  moveconns(j,i);
  adflist(j,&mvconn[upptr]);
  gralive[j] = 1;
  grlv[j] = FALSE;
  killist(&grlbp[j]);
  army = grarmy[j];
  dellist(j,&armygroups[army]);
  grarmy[j] = NOARMY;
  armysize[army] -= grsize[j];
  if(army != grarmy[i])combinearmy(army,grarmy[i]);
  armysize[grarmy[i]] += grsize[j];
  addlist(i,&chgrp); /* must reeval libs of this whole group */
  
  lptr = grnbp[j];
  while(lptr != EOL){
     dellist(j,&grnbp[list[lptr]]);
     lptr = links[lptr];
     }
  killist(&grnbp[j]);
  
  lptr = grldr[j];
  while(lptr != EOL){
     dellist(j,&ldrflag[list[lptr]]);
     lptr = links[lptr];
     }
  killist(&grldr[j]);
  
  lptr = grpieces[i];
  while(mvnext[lptr] != -1)
     lptr = mvnext[lptr];
  mvnext[lptr] = grpieces[j];
  lptr = mvnext[lptr];
  while(lptr != -1){
     adlibs(mvs[lptr],i);
     board[mvs[lptr]] = i;
     ++grsize[i]; 
     cuscan(mvs[lptr],j); 
     lptr = mvnext[lptr]; 
     }
   if((gralive[i] & 31) == DEAD){
      if(grnbp[i] != EOL){
         g2 = list[grnbp[i]];
         for(ptr2 = links[grnbp[i]]; ptr2 != EOL; ptr2 = links[ptr2]){
            combinearmy(grarmy[list[ptr2]],grarmy[g2]);
            }
         }
      }
  } 
