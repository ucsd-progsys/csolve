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
  
  
  
  
void lresurrect(int g){     /* bring back group g */
   int lptr,s,i,ldtmp,sn,g2,c;
   c = grcolor[g];
   lptr = grpieces[g];
   while(lptr != -1){
       s = mvs[lptr];
       board[s] = g;
       i = fdir[s];
       for(ldtmp = ldir[i]; i < ldtmp; ++i ){
          sn = s + nbr[i];
          ++lnbf[sn][c];
          --lnbn[sn];
          dellist(s,&nblbp[sn]);
          g2 = board[sn];
          if(g2 == NOGROUP)continue;
          if(g2 == g)continue;
          if(dellist(s,&grlbp[g2])){
             grlibs[g2]--;
             }
          addlist(g2,&grnbp[g]);
          addlist(g,&grnbp[g2]);
          } 
       lptr = mvnext[lptr]; 
       }
   }
  
  
void ldndate(int dnptr){
   int i,s,sn,lptr,c,g,splflag,ptr,tmp,ldtmp,g2;
   kosquare = kosave[dnptr];
   s = mvs[dnptr];
   if(s == PASS){  /* pass */
      return;
      }
   g = board[s];
   c = grcolor[g];
   splflag = FALSE;
   board[s] = NOGROUP;
   if(mvconn[dnptr] != EOL){
      ptr = mvconn[dnptr];      /* bring back combined groups */
      g = lsplit(ptr,g);
      splflag = TRUE;
      }
   for(ptr = mvcapt[dnptr]; ptr != EOL; ptr = links[ptr]){
         g2 = list[ptr]; /* bring back killed groups */
             lresurrect(g2);  /* resurrect enemy group */
         }
  
   if(!splflag){        /* remove piece from group */
      grsize[g]--;
      if(grsize[g] == 0){  /* only one piece in group */
         --maxgr;           /* delete group */
         killist(&grlbp[g]);     /* return liberties to free list */
         lptr = grnbp[g];
         while(lptr != EOL){
            dellist(g,&grnbp[list[lptr]]);
            lptr = links[lptr];
            }
         if(grnbp[g] != EOL)
		 killist(&grnbp[g]); /* return neighbors to free list */
         }
      else {                  /* take piece away from group */
         grpieces[g] = mvnext[grpieces[g]]; 
         ptr = lbply[dnptr];
         while(ptr != EOL){ 
            dellist(list[ptr],&grlbp[g]); 
            --grlibs[g];
            ptr = links[ptr];
            } 
         killist(&lbply[dnptr]); 
         ptr = nbply[dnptr];
         while(ptr != EOL){ 
            tmp = list[ptr];
            dellist(tmp,&grnbp[g]); 
            dellist(g,&grnbp[tmp]); 
            ptr = links[ptr];
            } 
         if(nbply[dnptr] != EOL)
		 killist(&nbply[dnptr]); 
         }
      if(edge[s] < 2){
         i = fdir[s]; 
         for(ldtmp = ldir[i]; i < ldtmp; ++i){
            sn = s + nbr[i];
            --lnbf[sn][c];
            ++lnbn[sn]; 
            addlist(s,&nblbp[sn]);
            g = board[sn];
            if(g == NOGROUP)continue;
            if(addlist(s,&grlbp[g])){
              ++grlibs[g];
              }
            }
         }
      else{
         --lnbf[s+1][c];
         ++lnbn[s+1];
         addlist(s,&nblbp[s+1]);
         --lnbf[s-1][c];
         ++lnbn[s-1];
         addlist(s,&nblbp[s-1]);
         --lnbf[s+boardsize][c];
         ++lnbn[s+boardsize];
         addlist(s,&nblbp[s+boardsize]);
         --lnbf[s-boardsize][c];
         ++lnbn[s-boardsize];
         addlist(s,&nblbp[s-boardsize]);
         g = board[s+1];
         if(g != NOGROUP )
            if(addlist(s,&grlbp[g]))++grlibs[g];
         g = board[s-1];
         if(g != NOGROUP )
            if(addlist(s,&grlbp[g]))++grlibs[g];
         g = board[s+boardsize];
         if(g != NOGROUP )
            if(addlist(s,&grlbp[g]))++grlibs[g];
         g = board[s-boardsize];
         if(g != NOGROUP )
            if(addlist(s,&grlbp[g]))++grlibs[g];
         }
      }
   else {
      if(edge[s] < 2){
         i = fdir[s];
         for(ldtmp = ldir[i]; i < ldtmp; ++i){
            sn = s + nbr[i];
            --lnbf[sn][c];
            ++lnbn[sn];
            addlist(s,&nblbp[sn]);
            g = board[sn];
            if(grcolor[g] != 1-c)continue;
            if(addlist(s,&grlbp[g])){
              ++grlibs[g];
              }
            }
         }
      else{
         --lnbf[s+1][c];
         ++lnbn[s+1];
         addlist(s,&nblbp[s+1]);
         --lnbf[s-1][c];
         ++lnbn[s-1];
         addlist(s,&nblbp[s-1]);
         --lnbf[s+boardsize][c];
         ++lnbn[s+boardsize];
         addlist(s,&nblbp[s+boardsize]);
         --lnbf[s-boardsize][c];
         ++lnbn[s-boardsize];
         addlist(s,&nblbp[s-boardsize]);
         g = board[s+1];
         if(grcolor[g] == 1-c)
            if(addlist(s,&grlbp[g]))++grlibs[g];
         g = board[s-1];
         if(grcolor[g] == 1-c)
            if(addlist(s,&grlbp[g]))++grlibs[g];
         g = board[s+boardsize];
         if(grcolor[g] == 1-c)
            if(addlist(s,&grlbp[g]))++grlibs[g];
         g = board[s-boardsize];
         if(grcolor[g] == 1-c)
            if(addlist(s,&grlbp[g]))++grlibs[g];
         }
      }
   }
  
  
  
int lsplit(int ptr,int j) {
   int lptr,lptr2,ptr2,tlist,g2,gret,c; 
   tlist = EOL; 
   c = grcolor[list[ptr]];
   ptr2 = ptr;
   while(ptr2 != EOL){
      g2 = list[ptr2];
      if(grcolor[g2] != c)break;
      adflist(list[ptr2],&tlist); 
      ptr2 = links[ptr2];
      } 
   ptr = tlist; 
   gret = list[tlist];
   killist(&grlbp[j]); 
   lptr = grnbp[j];
   while(lptr != EOL){
      dellist(j,&grnbp[list[lptr]]);
      lptr = links[lptr];
      }
   if(grnbp[j] != EOL)
	   killist(&grnbp[j]);
  
   --maxgr;              /* group j is gone! */
  
   lptr = mvnext[grpieces[j]];   /* unlink pieces list */
   ptr2 = ptr;
   g2 = list[ptr2];
   ptr2 = links[ptr2];
   lptr2 = grpieces[list[ptr2]];
   while(lptr != -1){
      board[mvs[lptr]] = g2;
      if(mvnext[lptr] == lptr2){
         mvnext[lptr] = -1;
         lptr = lptr2;
         g2 = list[ptr2]; 
         ptr2 = links[ptr2]; 
         if(ptr2 == EOL)
            lptr2 = -2; 
         else 
            lptr2 = grpieces[list[ptr2]]; 
         }
      else lptr = mvnext[lptr]; 
      } 
  
  
   ptr2 = ptr;
   while(ptr2 != EOL){   /* fix neighbors */
      g2 = list[ptr2];
      lptr = grnbp[g2]; 
      ptr2 = links[ptr2];
      while(lptr != EOL){ 
         addlist(g2,&grnbp[list[lptr]]);
         lptr = links[lptr];
         }
      }
   killist(&tlist);
   return(gret);
   }
  
  
/* set ladder flags at spots where moves will affect this ladder
 * ladder flags have places plays were made in a ladder and places where
 * first stone is in adjacent groups 
 * (should be only adjacent groups that were on board when ladder started)
 */

void upldrflags(int upptr,int ldrno){
	int s,sn,i,ldtmp,g2;
	s = mvs[upptr];
	if(s == PASS)return;

	if(addlist(ldrno,&ldrflag[s])){
       		adflist(s,&grldr[ldrno]);
       		}
	i = fdir[s];
	for(ldtmp = ldir[i]; i < ldtmp; ++i){
		sn = s + nbr[i];
		g2 = board[sn];
		if(g2 != NOGROUP){
			if(addlist(ldrno,&ldrflag[mvs[grpieces[g2]]]))
			   adflist(mvs[grpieces[g2]],&grldr[ldrno]);
			}
		}
	}
  
  
/* update data structures needed for tactical analyzer for move in 
 * move stack at
 * upptr.  
 * data structures updated are: kosquare, board[], lnbn[], lnbf[][], 
 * grpieces[], grsize[], grcolor[], grlibs[], grlbp[], nblbp[]
 *
 * ld[], lgr[], ltr1[], ltr2[], ltrgd[], army, and connection structures are
 * not modified
 *
 * return TRUE if move is legal
 */


int lupdate(int upptr){
    int i,c,sn,l,m,grp[4],lnew[4],nlist;
    int g,gnew,ldtmp,s;
    int g2;
    if(mvconn[upptr] != EOL)killist(&mvconn[upptr]);
    if(mvcapt[upptr] != EOL)killist(&mvcapt[upptr]);
    kosave[upptr] = kosquare;
    kosquare = NOSQUARE;
    s = mvs[upptr];
    if(s == PASS){  /* pass */
       return(TRUE);
       }
  
    ++numnodes;
    c = mvcolor[upptr];
    l = 0;
    m = 0;
    nlist = EOL;
    i = fdir[s];
    for(ldtmp = ldir[i]; i < ldtmp; ++i){   /* look at neighbors */
      sn = s + nbr[i];
      ++lnbf[sn][c];
      --lnbn[sn];
      dellist(s,&nblbp[sn]);
      g = board[sn];
      if(g == NOGROUP){   /* empty nbr, add a liberty */
         lnew[m++] = sn;
         }
      else if(grcolor[g] != c){      /* unfriendly neighbor */
         if(dellist(s,&grlbp[g])){
            if(--grlibs[g] == 0){  /* killed enemy group */
               lnew[m++] = sn;
               adflist(g,&mvcapt[upptr]);
               lkilgrp(g);
               }
            else grp[l++] = g; 
            }
         }
      else addlist(g,&nlist);      /* friendly neighbor */
      } 
    if(nlist == EOL){ 		/* create a new group */
         if(maxgr > NUMGROUPS - 2){ 
		    return(FALSE); /* probably need more work here */
            } 
         gnew = maxgr++;
         board[s] = gnew; 
         grsize[gnew] = 1;
         grcolor[gnew] = c;
         grlibs[gnew] = m;

         for(i = 0; i < l; ++i){
            g = grp[i];
            addlist(g,&grnbp[gnew]);
            addlist(gnew,&grnbp[g]);
            }
         for(i = 0; i < m; ++i)addlist(lnew[i],&grlbp[gnew]);
         mvnext[upptr] = -1;
         grpieces[gnew] = upptr;
         if(grlibs[gnew] == 1 && mvcapt[upptr] != EOL &&
            grsize[list[mvcapt[upptr]]] == 1 &&
            links[mvcapt[upptr]] == EOL)
            kosquare = list[grlbp[gnew]];
         }
   else {
      if(links[nlist] == EOL){ /* add stone to group */
         gnew = list[nlist];
         for(i = 0; i < l; ++i){
            g2 = grp[i];
            if(addlist(g2,&grnbp[gnew])){
               addlist(gnew,&grnbp[g2]);
               adflist(g2,&nbply[upptr]);
               }
            }
         for(i = 0; i < m; ++i)if(addlist(lnew[i],&grlbp[gnew])){
            ++grlibs[gnew];
            adflist(lnew[i],&lbply[upptr]);
            }
         }
      else{            /* combine groups */
         gnew = lcombine(nlist,upptr);
         for(i = 0; i < l; ++i){
            g2 = grp[i];
            if(addlist(g2,&grnbp[gnew])){
               addlist(gnew,&grnbp[g2]);
               }
            }
         for(i = 0; i < m; ++i)if(addlist(lnew[i],&grlbp[gnew]))
            ++grlibs[gnew];
         }
      board[s] = gnew;
      ++grsize[gnew];
      mvnext[upptr] = grpieces[gnew];
      grpieces[gnew] = upptr;
      --grlibs[gnew];
      dellist(s,&grlbp[gnew]);
      killist(&nlist);
      }
    if(grlibs[gnew] == 0)return(FALSE);  /* illegal suicide */
    return(TRUE);
    } 
  
  
  
void lkilgrp(int g){
   int lptr,s,i,sn,g2,ldtmp,c;
   c = grcolor[g];
   lptr = grnbp[g]; 
   while(lptr != EOL){
      dellist(g,&grnbp[list[lptr]]);
      lptr = links[lptr];
      } 
   lptr = grpieces[g];
   killist(&grnbp[g]); 
   while(lptr != -1){
      s = mvs[lptr];
      board[s] = NOGROUP; 
      i = fdir[s];
      for(ldtmp = ldir[i]; i < ldtmp; ++i){ 
         sn = s + nbr[i]; 
         --lnbf[sn][c];
         ++lnbn[sn];
         addlist(s,&nblbp[sn]);
         g2 = board[sn];
         if(g2 == NOGROUP)continue; 
         if(g2 == g)continue; 
         if(addlist(s,&grlbp[g2])){ 
           ++grlibs[g2];
           }
         }
      lptr = mvnext[lptr];
      }
  }
  
  
  
  
int lcombine(int nlist,int upptr){
  int lptr,g,nptr,g2;

  g = maxgr++;
  grsize[g] = 0;
  nptr = nlist;
  while(nptr != EOL){
      g2 = list[nptr];
      nptr = links[nptr];
      grsize[g] += grsize[g2];
      adflist(g2,&mvconn[upptr]);
      lptr = grnbp[g2];
      while(lptr != EOL){
         dellist(g2,&grnbp[list[lptr]]);
         addlist(g,&grnbp[list[lptr]]);
         lptr = links[lptr];
         }
      }
   g2 = list[nlist];
   grcolor[g] = grcolor[g2];
   cpylist(grlbp[g2],&grlbp[g]);
   grlibs[g] = grlibs[g2];
   cpylist(grnbp[g2],&grnbp[g]);
   grpieces[g] = grpieces[g2];
   lptr = grpieces[g];
   nptr = links[nlist];
   while(nptr != EOL){
      g2 = list[nptr];
      grlibs[g] += mrglist(grlbp[g2],&grlbp[g]);
      mrglist(grnbp[g2],&grnbp[g]); 
      while(mvnext[lptr] != -1){ 
         board[mvs[lptr]] = g;
         lptr = mvnext[lptr]; 
         }
      board[mvs[lptr]] = g; 
      mvnext[lptr] = grpieces[g2];
      lptr = grpieces[g2];
      nptr = links[nptr];
      }
   while(lptr != -1){
      board[mvs[lptr]] = g;
      lptr = mvnext[lptr];
      }
  
  
  
   return(g);
  }
  
