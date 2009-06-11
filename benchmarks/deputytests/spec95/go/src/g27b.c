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
  
  
int opdir[] = { DOWN,RIGHT,LEFT,UP };   /* opposit direction for a direction */
  
  
int dirnm[52] = { /* numbers for directions  0 = -19, 1 = -1, 2 = 1, 3 = 19 */
2,3,
1,3,
2,0,
1,0,
1,2,3,
0,3,2,
0,3,1,
1,2,0,
2,3,1,0,
1,3,2,0,
2,0,1,3,
1,0,2,3,
1,2,0,3,
0,3,1,2,
0,3,2,1,
1,2,3,0  }; 
  
  
  
  
  
void upxy(int s){     /* add stone at s to ltrxy */ 
   int i,j,offs,g,oldedge,sn;
   int x,y,ldtmp,dir; 
   g = board[s];

   if(edge[s] < 4){
      x = xval[s];
      y = yval[s];
      if(x < xmin)xmin = x;
      if(y < ymin)ymin = y;
      if(x > xmax)xmax = x;
      if(y > ymax)ymax = y;
      }
   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      offs = nbr[i];
      sn = s;
      oldedge = edge[s];
      dir = opdir[dirnm[i]];
      for(j = 0; j < 4; ++j){
         sn += offs;
         if(ld[sn] != 0 && ld[sn] != NOLD){  /* found a liberty */
            if(sqrbrd[sn][dir] != NOSQUARE && 
              dstbrd[sn][dir] == 1 )
               brklks(sn,sqrbrd[sn][dir],s,board[sqrbrd[sn][dir]]);
            if(j == 1 )
               addlks(sn,g,NOSQUARE);  /* fix links */
            if(sqrbrd[sn][dir] != NOSQUARE && 
              (dstbrd[sn][dir] == 2  ))
               brklkgs(sn,sqrbrd[sn][dir],s,board[sqrbrd[sn][dir]]);
            if(j == 2 )
               addlkgs(sn,g,NOSQUARE);  /* fix links */
            }
         sqrbrd[sn][dir] = s;
         dstbrd[sn][dir] = j;
         if(board[sn] != NOGROUP)break;
         if(ld[sn] > 1 && ld[sn] < 9){
            addlist(sn,&terhd);
            }
         if(edge[sn] < 4){
            x = xval[sn];
            y = yval[sn];
            if(x < xmin)xmin = x;
            if(y < ymin)ymin = y;
            if(x > xmax)xmax = x;
            if(y > ymax)ymax = y;
            }
         if(edge[sn] == 0)break;
         if(edge[sn] == 1 && oldedge == 2)break;
         oldedge = edge[sn];
         }
      } 
   }
  
  
  
  
void dnxy(int s,int noc) {
   int end,dir,dst,ldtmp,sqr;
   int i,j,sn,offs,oldedge,x,y; 

   if(edge[s] < 4){
      x = xval[s];
      y = yval[s];
      if(x < xmin)xmin = x;
      if(x > xmax)xmax = x;
      if(y < ymin)ymin = y;
      if(y > ymax)ymax = y;
      }
   i = fdir[s];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      sn = s;
      offs = nbr[i];
      dir = opdir[dirnm[i]];
      oldedge = edge[s];
      for(j = 0; j < 4; ++j){  /* delete xy refs to stone removed */
         sn = sn + offs;
         if(ld[sn] != 0 && ld[sn] != NOLD){
            addlist(sn,&terhd);
            if((j == 1  ) && noc == NOCOLOR)
               brklks(sn,s,NOSQUARE,board[s]);
            if((j == 2) && noc == NOCOLOR)
               brklkgs(sn,s,NOSQUARE,board[s]);
            }
         sqrbrd[sn][dir] = NOSQUARE;
         dstbrd[sn][dir] = 0;
         if(board[sn] != NOGROUP)break;
         if(edge[sn] < 4){
            x = xval[sn];
            y = yval[sn];
            if(x < xmin)xmin = x;
            if(x > xmax)xmax = x;
            if(y < ymin)ymin = y;
            if(y > ymax)ymax = y;
            }
         if(edge[sn] == 0)break;
         if(edge[sn] == 1 && oldedge > 1)break;
         }
      }
  
/*   if(ltrxy[s] == 0)return;  efficiency hack? */
   if(edge[s] == 0)return;
   i = fdir[s];
   end = ldir[i];
   if(edge[s] == 1)--end; 
   for(; i < end; ++i){       /* propagate xy past stone removed */
      dir = opdir[dirnm[i]];
      sqr = sqrbrd[s][dir];
      if(sqr == NOSQUARE)continue;
      dst = dstbrd[s][dir];
      sn = s; 
      offs = nbr[i];
      oldedge = edge[sn]; 

      for(j = dst+1; j < 4; ++j){ 
         sn += offs;
         if(ld[sn] != 0 && ld[sn] != NOLD && (j == 1) &&
             grcolor[board[sqr]] != noc)
            addlks(sn,board[sqr],s);
         if(ld[sn] != 0 && ld[sn] != NOLD && (j == 2) &&
             grcolor[board[sqr]] != noc)
            addlkgs(sn,board[sqr],s);
         sqrbrd[sn][dir] = sqr;
         dstbrd[sn][dir] = j;
         if(board[sn] != NOGROUP)break; 
         if(edge[sn] == 0)break;
         if(edge[sn] == 1 && oldedge > 1)break;
         }
      } 
  
   }
  
