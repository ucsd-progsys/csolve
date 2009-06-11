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
  
  
void dnn1(int s) {
   int sn,i,ldtmp;
   i = fdir[s]; 
   for(ldtmp = ldir[i]; i != ldtmp; ++i ){
      sn = s + nbr[i];
      if(ld[sn] > 1 && ld[sn] < 9 && grcolor[lgr[sn]] == 
          grcolor[lgr[s]]){     /* decn1 */ 
          --ld[sn]; 
          if(ld[sn] == 5)addlist(sn,&eyelist);
          } 
      } 
   }
  
void upn1(int s,int s1){
   int sn,i,ldtmp; 

   ld[s] = 0; 
   if(edge[s] <= 1)ld[s] += 2;
   if(edge[s] == 0)ld[s] += 2;
   i = fdir[s]; 
   for(ldtmp = ldir[i]; i != ldtmp; ++i ){
      sn = s + nbr[i];
      if(sn == s1)continue; 
      if(ld[sn] == 0){
         ld[s] += 2;
         if(ld[s] > 5)addlist(s,&eyelist); 
         }
      if(ld[sn] > 1 && ld[sn] < 9 && grcolor[lgr[sn]] == 
         grcolor[lgr[s]]){
            ++ld[s];
            if(ld[s] > 5)addlist(s,&eyelist); 
            } 
      } 
   i = fdir[s]; 
   for(ldtmp = ldir[i]; i != ldtmp; ++i ){
      sn = s + nbr[i];
      if(sn == s1)continue; 
      if(ld[sn] > 1 && ld[sn] < 9 && grcolor[lgr[sn]] == 
         grcolor[lgr[s]])incn1(sn); 
      } 
   }
  
void uscan(int s){ /* update lb for move at x,y. */ 
   int sn,i,sn2,g; 
   int j,ldtmp,ldtm2;
   if(ld[s] > 1 && ld[s] < 9){ 
      dnn1(s);
      } 
   if(ld[s] > 5 && ld[s] < NOLD)addlist(s,&eyelist);
   ld[s] = 0; 
   g = board[s];
   lgr[s] = g;
   i = fdir[s]; 
   for(ldtmp = ldir[i]; i != ldtmp; ++i){ 
      sn = s + nbr[i];
      if(ld[sn] == NOLD){     /* new n1 */
         lgr[sn] = g; 
         ld[sn] = 2;
         if(edge[sn] <= 1)ld[sn] += 2;
         if(edge[sn] == 0)ld[sn] += 2;
         j = fdir[sn];
         for(ldtm2 = ldir[j]; j != ldtm2; ++j){ 
            sn2 = sn + nbr[j];
            if(ld[sn2] > 1 && ld[sn2] < 9 && 
               grcolor[lgr[sn2]] == grcolor[g]){
               incn1(sn2);
               ++ld[sn];
               }
            } 
         if(ld[sn] > 5)addlist(sn,&eyelist);
         }
      else if(ld[sn] >= 2){     /* old n1 */
	      ld[sn] += 2;
	      if(ld[sn] > 5)addlist(sn,&eyelist);
	      if(grcolor[lgr[sn]] != grcolor[board[s]]){ 
		      dnn1(sn); 
		      if(ld[sn] > 5)addlist(sn,&eyelist); 
		      ld[sn] = NEUTRALLD;
		      } 
	      }
      } 
   }
  
  
void incn1(int s){
   ++ld[s]; 
   if(ld[s] >= 6) 
      addlist(s,&eyelist);
   }
  
  
  
  
  
void dscan(int s){
   int sn,i,j,sn2,g,s1,k; 
   int c,ldtm2,ldtmp;
   g = board[s];
   i = fdir[s]; 
   for(ldtmp = ldir[i]; i != ldtmp; ++i ){
      sn = s + nbr[i];
      if(ld[sn] == 2){     /* n1 going away */
         ld[sn] = NOLD; 
         tscr -= terv[sn];
         terv[sn] = 0;
         lgr[sn] = NOGROUP;
         }
      else if(ld[sn] > 1 && ld[sn] < 9){ 
         dnn1(sn);
         if(ld[sn] > 5)addlist(sn,&eyelist);
         ld[sn] = NOLD; 
         j = fdir[sn];
         for(ldtm2 = ldir[j]; j != ldtm2; ++j ){
            s1 = sn+nbr[j]; 
            if(s1 == s)continue;
            if(ld[s1] == 0){              /* dont worry about making -1 */
               lgr[sn] = lgr[s1]; 
               ld[sn] = 10;
               break; 
               }
            } 
         if(ld[sn] == 10){ 
            upn1(sn,s); 
            } 
         else {            /* n1 goes away */ 
            lgr[sn] = NOGROUP; 
            tscr -= terv[sn]; 
            terv[sn] = 0; 
            } 
         }
      else if(ld[sn] == NEUTRALLD){       /* fix the neutral lds */ 
         k = fdir[sn];
         for(ldtm2 = ldir[k]; k != ldtm2; ++k ){
            sn2 = sn + nbr[k];
            if(sn2 == s)continue; 
            if(ld[sn2] != 0)continue; 
            if(grcolor[lgr[sn2]] == 
               grcolor[g]){ 
               lgr[sn] =  lgr[sn2]; 
               ld[sn] = NEUTRALLD; 
               break; 
               }
            else if(ld[sn] == NEUTRALLD){
                lgr[sn] = lgr[sn2]; 
                ld[sn] = 10; 
                } 
            } 
         if(ld[sn] == 10){   /* neutral changes to n1 */
            upn1(sn,s); 
            } 
         }
      } 
   i = fdir[s]; 
   for(ldtmp = ldir[i]; i != ldtmp; ++i ){   /* now do square stone was on */ 
      c = lgr[s]; 
      sn = s + nbr[i];
      if(ld[sn] == 0){
         if(ld[s] == 0){
            ld[s] = 10;
            lgr[s] = lgr[sn]; 
            } 
         else if(grcolor[c] != grcolor[lgr[sn]]){ 
            ld[s] = NEUTRALLD; 
            } 
         }
      } 
   if(ld[s] == 0){    /* no neighbors, not an n1 */ 
      ld[s] = NOLD; 
      lgr[s] = NOGROUP;
      tscr -= terv[s];
      terv[s] = 0;
      } 
   else if(ld[s] == 10){ /* new n1 since has neighbor */ 
      upn1(s,NOSQUARE); 
      } 
   }
  
  
  
void cscan(int s,int g){
/* subtract n1 and n2 for split */ 
   int sn,i,ldtmp;
   lgr[s] = board[s]; 
   i = fdir[s]; 
   for(ldtmp = ldir[i]; i != ldtmp; ++i ){
      sn = s + nbr[i];
      if(lgr[sn] == g)
         lgr[sn] = lgr[s];
      } 
   }
  
void cuscan(int s,int g) {
   int sn,i,g2,ldtmp;
   g2 = board[s]; 
   lgr[s] = g2; 
   i = fdir[s]; 
   for(ldtmp = ldir[i]; i != ldtmp; ++i){ 
      sn = s + nbr[i];
      if(ld[sn] == 0 || ld[sn] == NEUTRALLD || ld[sn] == NOLD)continue;
  
      if(lgr[sn] != g)continue; 
      lgr[sn] = g2; 
      } 
   }
  
