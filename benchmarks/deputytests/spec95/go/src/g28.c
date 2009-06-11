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
  
  
void dncons(int s,int g){    /* delete cons when stone removed */
   int i,sn,sn2,cflag,ldtmp,j,ldtm2,c,tmplist,tmplist2;

   c = grcolor[g];
   i = fdir[s];
   for(ldtmp = ldir[i]; i != ldtmp; ++i){
      sn = s + nbr[i];
      if(board[sn] != NOGROUP)continue;
      cflag = TRUE;
      j = fdir[sn];
      for(ldtm2 = ldir[j]; j != ldtm2; ++j ){
         sn2 = sn + nbr[j];
         if(sn2 == s)continue;
         if(board[sn2] == g){
            cflag = FALSE;
            continue;
            }
         }
  
      if(cflag && cnbrd[sn] != EOL) 
         brconn(g,sn);  /* true if deleted protected connection */
      if(cflag){
         tmplist = EOL;
	 tmplist2 = EOL;
         for(j = 0; j < 4; ++j){
            if(grcolor[board[sqrbrd[sn][j]]] == c &&
              (dstbrd[sn][j] == 1 ) &&
               addlist(board[sqrbrd[sn][j]],&tmplist))
                 realbrklink(g,board[sqrbrd[sn][j]],sqrbrd[sn][j],sn);
            if(grcolor[board[sqrbrd[sn][j]]] == c &&
              (dstbrd[sn][j] == 2 ) &&
               addlist(board[sqrbrd[sn][j]],&tmplist2))
                 realbrklkg(g,board[sqrbrd[sn][j]],sqrbrd[sn][j],sn);
	    }
          killist(&tmplist);
	 killist(&tmplist2);
         }
      } 
   }
  

void brkconns(int s){   /* break all connections through s */
   int i,ptr;

   for(ptr = cnbrd[s]; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      dellist(s,&cnptr[i]);
      cncnum[i]--;
      if(cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0){
	       delconnrec(i);
	       }
      else if(cncnum[i] == 1)
         addlist(i,&cnchgd);
      }
   for(ptr = lkbrd[s]; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      dellist(s,&cnlkptr[i]);
      cnlknum[i]--;
      if(cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0){
	       delconnrec(i);
	       }
      }
   for(ptr = llbrd[s]; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      dellist(s,&cnllptr[i]);
      cnllnum[i]--;
      if(cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0){

	       delconnrec(i);
	       }
      }
   killist(&llbrd[s]);
   killist(&cnbrd[s]);
   killist(&lkbrd[s]);

   }


  
  
  
int brconn(int g,int s){  /* break all connections to g through s */
   int i,ptr,tmplist,flag;

   flag = FALSE;
   tmplist = EOL;
   cpylist(cnbrd[s],&tmplist);
   for(ptr = tmplist; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      if(cngr1[i] == g || cngr2[i] == g){
         --cncnum[i];
         dellist(s,&cnptr[i]);
         dellist(i,&cnbrd[s]);
         if(cncnum[i] == 0 && cnlknum[i] == 0 && cnllnum[i] == 0){
	       delconnrec(i);
	       }
         else if(cncnum[i] == 1)
            addlist(i,&cnchgd);
         }
      }
   killist(&tmplist);
   return(flag);
   }


  
void moveconns(int g1,int g2){   /* move connections from g1 to g2. g1 ends up unconnected*/
   int ptr,ptr2,g3,g4,i,j,p2;

   for(ptr = grcnp[g1]; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      g3 = cngr1[i];
      if(g3 == g1)g3 = cngr2[i];

      if(g3 == g2){   /* would be conn from group to itself */
	 dellist(i,&grcnp[g2]);
	 if(grldr[NUMGROUPS+i] != EOL)kill_ldrflags(NUMGROUPS+i);
         for(p2 = cnptr[i]; p2 != EOL; p2 = links[p2]){
            dellist(i,&cnbrd[list[p2]]);
            }
         for(p2 = cnlkptr[i]; p2 != EOL; p2 = links[p2]){
            dellist(i,&lkbrd[list[p2]]);
            }
         for(p2 = cnllptr[i]; p2 != EOL; p2 = links[p2]){
            dellist(i,&llbrd[list[p2]]);
            }
         cncnum[i] = 0;
         cnlknum[i] = 0;
	 cnllnum[i] = 0;
         killist(&cnptr[i]);
         killist(&cnlkptr[i]);
	 killist(&cnllptr[i]);
	 adflist(i,&cnfreelist);
         continue;
         }
      g4 = NOGROUP;
      for(ptr2 = grcnp[g2]; ptr2 != EOL; ptr2 = links[ptr2]){
         /* see if g2 already has connection to same group */
         j = list[ptr2];
         g4 = cngr1[j];
         if(g4 == g2)g4 = cngr2[j];
         if(g4 == g3)break;
         }
      if(g4 != g3){  /* move connection as whole */
         if(cngr1[i] == g1)cngr1[i] = g2;
         else cngr2[i] = g2;
         addlist(i,&grcnp[g2]);
         }
      else { /* delete conn i. add to conn j */
         dellist(i,&grcnp[g3]);
         addlist(j,&grcnp[g3]);

	 if(grldr[NUMGROUPS+i] != EOL)kill_ldrflags(NUMGROUPS+i);
	 adflist(i,&cnfreelist);
         
         for(p2 = cnptr[i]; p2 != EOL; p2 = links[p2]){
            dellist(i,&cnbrd[list[p2]]);
            if(addlist(j,&cnbrd[list[p2]])){
               addlist(list[p2],&cnptr[j]);
               ++cncnum[j];
               }
            }
         cncnum[i] = 0;
         killist(&cnptr[i]);

         for(p2 = cnlkptr[i]; p2 != EOL; p2 = links[p2]){
            dellist(i,&lkbrd[list[p2]]);
            if(addlist(j,&lkbrd[list[p2]])){
               addlist(list[p2],&cnlkptr[j]);
               ++cnlknum[j];
               }
            }
         cnlknum[i] = 0;
         killist(&cnlkptr[i]);

         for(p2 = cnllptr[i]; p2 != EOL; p2 = links[p2]){
            dellist(i,&llbrd[list[p2]]);
            if(addlist(j,&llbrd[list[p2]])){
               addlist(list[p2],&cnllptr[j]);
               ++cnllnum[j];
               }
            }
         cnllnum[i] = 0;
         killist(&cnllptr[i]);
         }
      }
   killist(&grcnp[g1]);
   }
  
  
void adcons(int s,int g){       /* add connections through a point */
				/* but not to group g */
   int n[4],i,sn,j,k,ldtmp,c,dst;

   k = 0;
   i = fdir[s];
   for(ldtmp = ldir[i]; i != ldtmp; ++i){
      sn = s + nbr[i];
      if(board[sn] != NOGROUP && board[sn] != g)
         n[k++] = board[sn];
      }
   for(i = 0; i < k-1; ++i)
      for(j = i + 1; j != k; ++j)
         if(grcolor[n[i]] == grcolor[n[j]])
            addconn(n[i],n[j],s);
   for(i = 0; i < 4; ++i){
      sn = sqrbrd[s][i];
      dst = dstbrd[s][i];
      if(sn != NOSQUARE && (dst == 1 )){
         c = grcolor[board[sn]];
         for(j = 0; j < k; ++j)
            if(grcolor[n[j]] == c)
               addlink(n[j],board[sn],s);
         }
      if(sn != NOSQUARE && (dst == 2 )){
         c = grcolor[board[sn]];
         for(j = 0; j < k; ++j)
            if(grcolor[n[j]] == c)
               addlkg(n[j],board[sn],s);
         }
      }
   }
  
void addconn(int g1,int g2,int s){  /* add a connection from g1 to g2 at s */
   int i,ptr;
   if(g1 == g2)return;

   ptr = grcnp[g1];
   while(ptr != EOL){
      i = list[ptr];
      ptr = links[ptr];
      if(cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1){
         if(addlist(s,&cnptr[i])){
            ++cncnum[i];
            if(cncnum[i] < 3)
               addlist(i,&cnchgd);
            }
         addlist(i,&cnbrd[s]);
         return;
         }
      }
   i = gtflist(&cnfreelist);

   addlist(i,&cnchgd);
   addlist(s,&cnptr[i]);
   addlist(i,&cnbrd[s]);
   addlist(i,&grcnp[g1]);
   addlist(i,&grcnp[g2]);
   cngr1[i] = g1;
   cngr2[i] = g2;
   cncnum[i] = 1;
   cnprot[i] = CANT_CONNECT;
   if(grarmy[g1] == grarmy[g2]){
      cnprot[i] = AJI_CONNECT;
      }
   }
  
  
void chkcon(int s,int g){  /* add connections to g at libs next to s */
				/* s has a stone on it for group g */
   int i,sn,sn2,j,cflag,gflag,c;
   int grp[4],k,l,ldtmp,ldtm2;

   for(i = 0; i < 4; i++){
       grp[i] = -1;
       }
   c = grcolor[g];
   i = fdir[s];
   for(ldtmp = ldir[i]; i != ldtmp; ++i){
      sn = s + nbr[i];
      if(board[sn] != NOGROUP)continue;
      for(j = 0; j < 4; ++j){
         if(grcolor[board[sqrbrd[sn][j]]] == c &&
           (dstbrd[sn][j] == 1  ))
            addlink(g,board[sqrbrd[sn][j]],sn);
         if(grcolor[board[sqrbrd[sn][j]]] == c &&
           (dstbrd[sn][j] == 2  ))
            addlkg(g,board[sqrbrd[sn][j]],sn);
	 }
      cflag = FALSE;
      k = 0;
      j = fdir[sn];
      for(ldtm2 = ldir[j]; j != ldtm2; ++j){
         sn2 = sn + nbr[j];
         if(sn2 == s)continue;
         if(board[sn2] == NOGROUP)continue;
         gflag = FALSE;
         for(l = 0; l != k; ++l)
            if(grp[l] == board[sn2])gflag = TRUE;
         if(gflag)continue;
         if(board[sn2] != g){
            grp[k++] = board[sn2];
            }
         else cflag = TRUE;
         }
      if(!cflag)
         for(j = 0; j != k; ++j)
            if(grcolor[grp[j]] == c)addconn(grp[j],g,sn);
      }
   }
  

  

void addlks(int s,int g,int nos){ /* addlinks from g to groups adjacent to s */
				/* dont add links to nos */
   int c1,i,g2,ldtmp,sn;

   c1 = grcolor[g];
   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      sn = s + nbr[i];
      if(sn == nos)continue;
      g2 = board[sn];
      if(g2 == g || grcolor[g2] != c1)continue;
      addlink(g,g2,s);
      }
   }

void addlkgs(int s,int g,int nos){ /* addlkgs from g to groups adjacent to s */
				/* dont add links to nos */
   int c1,i,g2,ldtmp,sn;

   c1 = grcolor[g];
   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      sn = s + nbr[i];
      if(sn == nos)continue;
      g2 = board[sn];
      if(g2 == g || grcolor[g2] != c1)continue;
      addlkg(g,g2,s);
      }
   }


void brklks(int s,int sqr,int s2,int g){/* s is empty sqr has stone on it */
			/* break links from s to sqr. ignore group at s2 */
			/* g is group at sqr to break link to */
   int c2,i,g2,ldtmp,tmplist,ptr;
   tmplist = EOL;

   c2 = grcolor[g];
   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      if(s+nbr[i] == s2)continue;
      g2 = board[s+nbr[i]];
      if(grcolor[g2] == c2)
         addlist(g2,&tmplist);
      }
   for(ptr = tmplist; ptr != EOL; ptr = links[ptr])
      brklink(list[ptr],g,sqr,s);
   killist(&tmplist);
   }


void brklkgs(int s,int sqr,int s2,int g){ /* s is empty sqr has stone on it */
			/* break links from s to sqr. ignore group at s2 */
			/* g is group at sqr to break link to */
   int c2,i,g2,ldtmp,tmplist,ptr;
   tmplist = EOL;

   c2 = grcolor[g];
   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      if(s+nbr[i] == s2)continue;
      g2 = board[s+nbr[i]];
      if(grcolor[g2] == c2)
         addlist(g2,&tmplist);
      }
   for(ptr = tmplist; ptr != EOL; ptr = links[ptr])
      brklkg(list[ptr],g,sqr,s);
   killist(&tmplist);
   }





void rstrlks(int gnew,int gold,int s){  /* restore links from gold to gnew */
					/* s has stone on it. */
   int i,ldtmp,offs,oldedge,sn,j;

   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      sn = s;
      offs = nbr[i];
      oldedge = edge[s];
      for(j = 0; j < 3; ++j){
         sn = sn + offs;
         if(board[sn] != NOGROUP)break;
         if(j == 1 && ld[sn] != 0 && ld[sn] != NOLD){ /* found one */
            brklks(sn,s,NOSQUARE,gold);
            addlks(sn,gnew,NOSQUARE);
            }
         if(j == 2 && ld[sn] != 0 && ld[sn] != NOLD){ /* found one */
            brklkgs(sn,s,NOSQUARE,gold);
            addlkgs(sn,gnew,NOSQUARE);
            }
         if(edge[sn] == 0)break;
         if(edge[sn] == 1 && oldedge > 1)break;
         }
      }
   }
   





void brklink(int g1,int g2,int s1,int s2){ /* break link from g1 to g2 (s1 part of g2) at s2 */
/* don'y break it if multiple links same square */
   int i,j,ptr,ldtmp,cflag;

   if(g1 == g2)return;
   cflag = FALSE;
   i = fdir[s2];	/* see if any g2's next to s2 */
   for(ldtmp = ldir[i]; i < ldtmp; ++i)
      if(board[s2+nbr[i]] == g2){
         cflag = TRUE;
         break;
         }
      for(j = 0; j < 4; ++j){
         if(sqrbrd[s2][j] != s1 &&
            (board[sqrbrd[s2][j]] == g2 ||
             (cflag && board[sqrbrd[s2][j]] == g1)) && 
            (dstbrd[s2][j] == 1 /* || dstbrd[s2][j] == 2 */)){
            return;
            }
         }
   for(ptr = grcnp[g1]; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      if(cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1){
         if(!dellist(s2,&cnlkptr[i])){
            }
         else {
            --cnlknum[i];
            }
         dellist(i,&lkbrd[s2]);
         if(cnlknum[i] == 0 && cncnum[i] == 0 && cnllnum[i] == 0){

	    delconnrec(i);
            }
	 else if(cnlknum[i] < 3)
		 addlist(i,&cnchgd);
         return;
         }
      }
   }

void brklkg(int g1,int g2,int s1,int s2){ /* break link from g1 to g2 (s1 part of g2) at s2 */
/* don'y break it if multiple links same square */
   int i,j,ptr,ldtmp,cflag;

   if(g1 == g2)return;
   cflag = FALSE;
   i = fdir[s2];	/* see if any g2's next to s2 */
   for(ldtmp = ldir[i]; i < ldtmp; ++i)
      if(board[s2+nbr[i]] == g2){
         cflag = TRUE;
         break;
         }
      for(j = 0; j < 4; ++j){
         if(sqrbrd[s2][j] != s1 &&
            (board[sqrbrd[s2][j]] == g2 ||
             (cflag && board[sqrbrd[s2][j]] == g1)) && 
            (dstbrd[s2][j] == 2)){
            return;
            }
         }
   for(ptr = grcnp[g1]; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      if(cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1){
         if(!dellist(s2,&cnllptr[i])){
            }
         else {
            --cnllnum[i];
            }
         dellist(i,&llbrd[s2]);
         if(cnllnum[i] == 0 && cncnum[i] == 0 && cnlknum[i] == 0){
	    delconnrec(i);
            }
         else if(cnllnum[i] < 3)
               addlist(i,&cnchgd);
         return;
         }
      }
   }


void realbrklink(int g1,int g2,int s1,int s2){ /* break link from g1 to g2 (s1 part of g2) at s2 */
/* break it even if other conns */
   int i,j,ptr,ldtmp;

   if(g1 == g2)return;
   i = fdir[s2];	/* see if any g2's next to s2 */
   for(ldtmp = ldir[i]; i < ldtmp; ++i)
      if(board[s2+nbr[i]] == g2){	/* found g2 near. */
         for(j = 0; j < 4; ++j)	/* are any g1's far? */
            if(board[sqrbrd[s2][j]] == g1 &&
               (dstbrd[s2][j] == 1 /* || dstbrd[s2][j] == 2 */)){
               return;
               }
         break;
         }
   for(ptr = grcnp[g1]; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      if(cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1){
         if(!dellist(s2,&cnlkptr[i])){
            }
         else {
            --cnlknum[i];
             }
         dellist(i,&lkbrd[s2]);
         if(cnlknum[i] == 0 && cncnum[i] == 0 && cnllnum[i] == 0){

	    delconnrec(i);
            }
         else if(cnlknum[i] < 3)
               addlist(i,&cnchgd);
         return;
         }
      }
   }


void realbrklkg(int g1,int g2,int s1,int s2){ /* break link from g1 to g2 (s1 part of g2) at s2 */
/* break it even if other conns */
   int i,j,ptr,ldtmp;

   if(g1 == g2)return;
   i = fdir[s2];	/* see if any g2's next to s2 */
   for(ldtmp = ldir[i]; i < ldtmp; ++i)
      if(board[s2+nbr[i]] == g2){	/* found g2 near. */
         for(j = 0; j < 4; ++j)	/* are any g1's far? */
            if(board[sqrbrd[s2][j]] == g1 &&
               (dstbrd[s2][j] == 2 )){
               return;
               }
         break;
         }
   for(ptr = grcnp[g1]; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      if(cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1){
         if(!dellist(s2,&cnllptr[i])){
            }
         else {
            --cnllnum[i];
             }
         dellist(i,&llbrd[s2]);
         if(cnlknum[i] == 0 && cncnum[i] == 0 && cnllnum[i] == 0){
	    delconnrec(i);
            }
         else if(cnllnum[i] < 3)
               addlist(i,&cnchgd);
         return;
         }
      }
   }

void delconnrec(int i){
	dellist(i,&grcnp[cngr1[i]]);
	dellist(i,&grcnp[cngr2[i]]);
	adflist(i,&cnfreelist);
	if(grldr[NUMGROUPS+i] != EOL)kill_ldrflags(NUMGROUPS+i);
	if(cnprot[i] >= AJI_CONNECT){
		addlist(mvs[grpieces[cngr1[i]]],&charmy);
		addlist(mvs[grpieces[cngr2[i]]],&charmy);
		}
	}
	
void addlink(int g1,int g2,int s){  /* add a link from g1 to g2 at s */
   int i,ptr;

   if(g1 == g2)return;

   for(ptr = grcnp[g1]; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      if(cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1){
         if(addlist(s,&cnlkptr[i])){
            ++cnlknum[i];
            if(cnlknum[i] > 1)
               addlist(i,&cnchgd);
            }
         addlist(i,&lkbrd[s]);
         return;
         }
      }
   i = gtflist(&cnfreelist);

   addlist(i,&cnchgd);
   addlist(s,&cnlkptr[i]);
   addlist(i,&lkbrd[s]);
   addlist(i,&grcnp[g1]);
   addlist(i,&grcnp[g2]);
   cngr1[i] = g1;
   cngr2[i] = g2;
   cnlknum[i] = 1;
   cnprot[i] = CANT_CONNECT;
   if(grarmy[g1] == grarmy[g2])cnprot[i] = AJI_CONNECT;
   }



void addlkg(int g1,int g2,int s){  /* add a linkage from g1 to g2 at s */
   int i,ptr;
   if(g1 == g2)return;

   for(ptr = grcnp[g1]; ptr != EOL; ptr = links[ptr]){
      i = list[ptr];
      if(cngr1[i] == g1 && cngr2[i] == g2 ||
         cngr1[i] == g2 && cngr2[i] == g1){
         if(addlist(s,&cnllptr[i])){
            ++cnllnum[i];
            if(cnllnum[i] > 1)
               addlist(i,&cnchgd);
            }
         addlist(i,&llbrd[s]);
         return;
         }
      }
   i = gtflist(&cnfreelist);
   addlist(i,&cnchgd);
   addlist(s,&cnllptr[i]);
   addlist(i,&llbrd[s]);
   addlist(i,&grcnp[g1]);
   addlist(i,&grcnp[g2]);
   cngr1[i] = g1;
   cngr2[i] = g2;
   cnllnum[i] = 1;
   cnprot[i] = CANT_CONNECT;
   if(grarmy[g1] == grarmy[g2])cnprot[i] = AJI_CONNECT;
   }
