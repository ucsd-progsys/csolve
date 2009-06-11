/* Copyright 1993 David Fotland.
 * Permission granted to Spec to use and distribute as a computer benchamrk
 * permission is not granted to use or modify this code for any
 * purpose other than benchmarking or computer system performance tuning.
 *
 * This program is available (with a user interface) for:
 * IBM-PC/DOS from Ishi Press Inc San Jose CA (408)944-9900
 * Penpoint from PenGames San Jose Ca (408)985-1236
 */

#include <stdio.h>      // sm: printf

# include "g2hd.h"
# define B -1
# define W 1
# define NEUT 0
  
 
int ccl[] = { -1,1};
  
extern int ltrfac[NUM_LIVENESS_STATES];  
  
  
void upltr(void){     /* fix the ltr data structure */
   int s,x,y;

   --ymin; --xmin; ++ymax; ++xmax;
   if(ymin < 4)ymin = 0;
   if(xmin < 4)xmin = 0;
   if(ymax > boardsize-5)ymax = boardsize-1;
   if(xmax > boardsize-5)xmax = boardsize-1;
  
   if(xmax < 3)xmax = 3;
   if(xmin > boardsize-4)xmin = boardsize-4;
   if(ymax < 3)ymax = 3;
   if(ymin > boardsize-4)ymin = boardsize-4;
  

	/* clear out old values */
   for(x = xmin; x <= xmax; ++x)
      for(y = ymin; y <= ymax; ++y){
         s = y*boardsize+x;
         if(edge[s] > 4)continue;
         if(ltr1[s] != 0){
            if(ltr1[s] != 0 && ld[s] != 0 && ld[s] != NOLD){
               addlist(s,&terhd);
               }
            }
         ltrscr -= ltr2[s];
         ltr2[s] = 0;
         ltr1[s] = 0;
         ltrgd[s] = FALSE;
         }
  
 
   if(ymin == 0)chckside(xmin,xmax,boardsize,1,RIGHT,LEFT);
   if(ymax == boardsize-1)
      chckside(xmin+boardsquare-boardsize,xmax+boardsquare-boardsize,
               -boardsize,1,RIGHT,LEFT);
   if(xmin == 0)
      chckside(ymin*boardsize,ymax*boardsize,1,boardsize,DOWN,UP);
   if(xmax == boardsize-1)
      chckside(ymin*boardsize+boardsize-1,ymax*boardsize+boardsize-1,
              -1,boardsize,DOWN,UP);



   if(xmin == 0 && ymin == 0)   /* corner */
	   chckcorner(0,1,boardsize);
   if(xmax == boardsize-1 && ymin == 0)   /* corner */
	   chckcorner(boardsize-1,-1,boardsize);
   if(xmin == 0 && ymax == boardsize-1)   /* corner */
	   chckcorner(boardsquare-boardsize,1,-boardsize);
   if(xmax == boardsize-1 && ymax == boardsize-1)   /* corner */
	   chckcorner(boardsquare-1,-1,-boardsize);
   xmin = ymin = 18;
   xmax = ymax = 0;
   }

/* adjust corner territory.  cs is corner square.  d1 and d2 are directions */

void chckcorner(int cs,int d1,int d2){
	int n = 0,tmp;
        tmp = cs + (d1+d2)*2;
	if(ltr1[cs+d1*2] != 0 && ltr1[cs+d2*2] != 0 && 
	   ld[tmp] > 1 && ld[tmp] != NOLD){
		if(ltr1[cs] == 0 &&
		   ld[cs] != 0){
			++n;
			ltr1[cs] = 9;
			}
		if(ltr1[cs+d1] == 0 &&
		   ld[cs+d1] != 0){
			++n;
			ltr1[cs+d1] = 9;
			}
		if(ltr1[cs+d2] == 0 &&
		   ld[cs+d2] != 0){
			++n;
			ltr1[cs+d2] = 9;
			}
		if(ltr1[cs+d1+d2] == 0 &&
		   ld[cs+d1+d2] != 0){
			++n;
			ltr1[cs+d1+d2] = 9;
			}
		ltr1[tmp] += n;
		}
	}

	
// sm: rectify erroneous array access
int bugfixIndex(int index)
{
  if (index < 0) {
    //fprintf(stderr, "bad array index: %d\n", index);
    return 0;        // use this index instead
  }
  else {
    return index;    // index ok
  }
}


/* return TRUE if territory is undercuttable with hane or similar move */
/* includes if can be ataried */
/* dir is towards center, dir2 is along edge, c is color of territory */

int edgeofterr(int s,int dir,int dir2,int c){
	int sn,ptr;
	sn = s + dir2 + dir;
	if(board[s+dir2] != NOGROUP){
		if(grcolor[board[s+dir2]] == 1-c &&
		   gralive[board[s+dir2]] == DEAD)return(FALSE);
		if(grlibs[board[s+dir2]] == 1)return(TRUE);
		if(grsize[board[s+dir2]] == 1 &&
		   grlibs[board[s+dir2]] == 2 &&
		   edge[s+dir2] > 1){
			for(ptr = grlbp[board[s+dir2]]; ptr != EOL; ptr = links[ptr])
				if(list[ptr] != s && lnbn[list[ptr]] > 1)return(TRUE);
			}
		}
	else {  /* open spot to side of terr */
		if(board[sn] == NOGROUP){  /* empty spot to side of stone */
			if(edge[s] == 1 &&
			   grcolor[board[sn+dir]] == 1-c &&
			   gralive[board[sn+dir]] != DEAD)return(TRUE);
			if(edge[sn] > 1 &&  board[sn-dir+dir2] == NOGROUP){
				if(gralive[board[sn+dir2]] != DEAD &&
				   grcolor[board[sn+dir2]] == 1-c)
					return(TRUE);
				if(edge[sn+dir2] > 1 && board[bugfixIndex(sn-dir+dir2+dir2+dir2)] == NOGROUP &&
				   gralive[board[sn+dir2+dir2]] != DEAD &&
				   grcolor[board[sn+dir2+dir2]] == 1-c)
					return(TRUE);
				}
			}
		else {
			if(gralive[board[sn]] != DEAD && grcolor[board[sn]] == 1-c)
			return(TRUE);
			if(edge[sn] > 1 && grcolor[board[sn]] == c &&
			   ld[sn-dir+dir2] != 0 && 
			   grcolor[board[sn+dir2]] == 1-c &&
			   gralive[board[sn+dir2]] != DEAD)return(TRUE);
			}
		}
	return(FALSE);
	}


/* do territory for a side.  start, stop, run along the edge
 * of the side.  dir is direction towards the center as offset.  dir2
 * is direction along edge as offset.  udir and udir2 are directions
 * along edge as indeces.  dir2 and udir are the same direction.
 */  



void chckside(int start,int stop,int dir,int dir2,int udir,int udir2){
     int s,sn,g,c,x,gral,n,i,stone_above,between_stones;
     int ptr,ptr2,ldtmp,touched[10],goodflag;
     
     for(x = start; x <= stop; x += dir2){
        goodflag = 0;
        n = 0;
        ptr = 0;
        s = x;
        if(ld[s] == 0)continue;	/* stone on edge */
 				/* undercut? */

        if(ld[s] != NOLD){	/* found liberty on first line */
           g = lgr[s];
           c = grcolor[g];
           if(ld[s] == NEUTRALLD && gralive[g] != DEAD){
              goodflag = 2;
              }
	   else if(grcolor[lgr[s+dir]] == 1-c &&
		gralive[lgr[s+dir]] != DEAD && gralive[g] != DEAD)
		goodflag = 2;
           else if(ld[s+dir] == NEUTRALLD)
		goodflag = 2;

           else if(ld[s+dir] != 0 && ld[s] == 4 && edge2[s] > 3)goodflag = 2;
           else if(ld[s+dir] == NOLD && ld[s] == 4 && edge2[s] > 3)goodflag = 2;
	   else if(grthreatened[lgr[s]])goodflag = 2;
           else if(edge[s] != 0 && edgeofterr(s,dir,dir2,c))goodflag = 3;
           else if(edge[s] != 0 && edgeofterr(s,dir,-dir2,c))goodflag = 3;
	   if(goodflag != 2)goodflag += undercut(s,-dir,dir2,c,udir,udir2);
           gral = gralive[g] & 31;
           if(ltr1[s] == 0)++n;
           c = ccl[c];
           i = fdir[s];
           for(ldtmp = ldir[i]; i < ldtmp; ++i){
               sn = s + nbr[i];
               if(ld[sn] != 0)continue;
               if(gralive[board[sn]] < SMOTHERED)
                  gralive[board[sn]] |= 1024;
               }
           if(ltr1[s] == 0)ltrgd[s] = goodflag;
           else ltrgd[s] |= goodflag;
           ltr1[s] += n;
           if(!goodflag){
              ltrscr += n*ltrfac[gral] * c;
              ltr2[s] += n * ltrfac[gral] * c;
              }
           if(terv[s] != 0)addlist(s,&terhd);
           n = 0;
           }
        if(ltr1[s] == 0){
           ++n;
           touched[ptr++] = s;
           }

        s = s+dir;
        if(ld[s] == 0)continue;	/* ran into stone */

        if(ld[s] != NOLD){  /* found liberty on second line */
           g = lgr[s];
           c = grcolor[g];
           goodflag = 0;
           stone_above = ld[s+dir] == 0 || (ld[s+dir] >= 5 && 
              lnbf[s+dir][c] != 0);
           between_stones = 
              (edge[s] > 1 && ld[s+dir2] != NEUTRALLD && 
               ld[s-dir2] != NEUTRALLD && 
	       (lnbf[s+dir2][c] != 0 || ld[s+dir2] == 0) && 
		(lnbf[s-dir2][c] != 0 || ld[s-dir2] == 0));
           if(ld[s] == NEUTRALLD && gralive[g] != DEAD){
              goodflag = 2;
              }
	   else if(grthreatened[lgr[s]])goodflag = 2;
           else if(!stone_above && !between_stones && edge2[s] > 3)goodflag = 1;
           else if(edge[s] != 1 && edgeofterr(s,dir,dir2,c))goodflag = 3;
           else if(edge[s] != 1 && edgeofterr(s,dir,-dir2,c))goodflag = 3;
	   if(goodflag != 2)goodflag += undercut(s,-dir,dir2,c,udir,udir2);
           gral = gralive[g] & 31;
           if(ltr1[s] == 0)++n;
           c = ccl[grcolor[g]];
           for(ptr2 = 0; ptr2 < ptr; ptr2++){
              ltr1[touched[ptr2]] = 9;
	      ltrgd[touched[ptr2]] = goodflag;
	      }
           ptr = 0;
           i = fdir[s];
           for(ldtmp = ldir[i]; i < ldtmp; ++i){
              sn = s + nbr[i];
              if(gralive[board[sn]] < SMOTHERED)
                 gralive[board[sn]] |= 1024;
              }
           if(ltr1[s] == 0)ltrgd[s] = goodflag;
           else ltrgd[s] |= goodflag;
           ltr1[s] += n;
           if(!goodflag){
              ltrscr += n * ltrfac[gral] * c;
              ltr2[s] += n * ltrfac[gral] * c;
              }
           if(terv[s] != 0)addlist(s,&terhd);
           n = 0;
           }
        if(ltr1[s] == 0){
           ++n;
           touched[ptr++] = s;
           }

        s += dir;
        if(ld[s] == 0)continue;

        if(ld[s] != NOLD){  /* found liberty on third line */
           g = lgr[s];
           c = grcolor[g];
           
           goodflag = 0;
           stone_above = ld[s+dir] == 0 || (ld[s+dir] >= 5 && 
              lnbf[s+dir][c] != 0);
	   between_stones = edge[s] > 1 && ld[s+dir] != NEUTRALLD &&
		((ld[s-dir2] == 0 && ld[s+dir2] != NEUTRALLD && 
		lnbf[s+dir2][c] != 0) ||
		(ld[s+dir2] == 0 && ld[s-dir2] != NEUTRALLD &&
		lnbf[s-dir2][c] != 0) ||
		 ld[s-dir2] == 0 && ld[s+dir2] == 0);
           if(ld[s] == NEUTRALLD && gralive[g] != DEAD || grthreatened[g])
              goodflag = 2;
           else if(!stone_above && !between_stones && edge2[s] > 3)goodflag = 1;
           else if(edge[s] != 1 && edgeofterr(s,dir,dir2,c))goodflag = 3;
           else if(edge[s] != 1 && edgeofterr(s,dir,-dir2,c))goodflag = 3;
	   if(goodflag != 2)goodflag += undercut(s,-dir,dir2,c,udir,udir2);
       /* don't reach around corner 
           if(ld[s] != NEUTRALLD && ucut[1-c][0] && (edge[s] == 3 ||
                n != 0))goodflag += 4;
           if(ld[s] != NEUTRALLD && ucut[1-c][1] && (edge[s] == 3 ||
                n != 0))goodflag += 4;
	*/
           gral = gralive[g] & 31;
           if(ltr1[s] == 0)++n;
           c = ccl[grcolor[g]];
           for(ptr2 = 0; ptr2 < ptr; ptr2++){
              ltr1[touched[ptr2]] = 9;
	      ltrgd[touched[ptr2]] = goodflag;
		}
           ptr = 0;
           i = fdir[s];
           for(ldtmp = ldir[i]; i < ldtmp; ++i){
              sn = s + nbr[i];
              if(gralive[board[sn]] < SMOTHERED)
                 gralive[board[sn]] |= 1024;
              }
           if(ltr1[s] == 0)ltrgd[s] = goodflag;
           else ltrgd[s] |= goodflag;
           ltr1[s] += n;
           if(!goodflag){
              ltrscr += n * ltrfac[gral] * c;
              ltr2[s] += n * ltrfac[gral] * c;
              }
           if(terv[s] != 0)addlist(s,&terhd);
           n = 0;
           }
        s += dir;
        if(ld[s] == 0)continue;
	if(edge[s] != 4)continue;

        if(ld[s] != NOLD){  /* found liberty on 4th line */
	   g = lgr[s];
	   c = grcolor[g];
	   goodflag = 1;
	   if(ld[s] == NEUTRALLD)goodflag = 2;
           else {
	   	goodflag += undercut(s,-dir,dir2,c,udir,udir2);
		}
	   ltrgd[s] = goodflag;
           }
        }
     }
  
