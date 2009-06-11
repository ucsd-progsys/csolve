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

# define MIN(x,y) (((x) < (y)) ? x : y)  
  
int c8[] = { 1,1,1,4,1,4,8,8 };
  
int c7[] = { 1,1,1,1,1,1,4,4 };

int cpot[] = { 0,0,0,0,0,0,8,8 };  
  
extern int msqr,mclr;
  

/* eyes are described in the eyerecord data structure. an eye is a dead
 * group or a small area with at least one point surrounded on 3 or 4
 * sides.
 * eye values range from 2 to 8 with 8 being a complete eye
 * eyelist has all potential eyes which could have become not
 * eyes and all potential new eyes and all eyes which have to be reevaled.
 * this is all points whose ld values changed from 5 or less to 6 or more
 * or vice versa. 
 *
 * there is also the eyerec data structure which handles eyes larger than one
 * point and keeps track of vital points
 */  
  
void fixli(void){
   int s,c,ptr,g;
   int sopen;

   findeyelist();

   for(ptr = eyelist; ptr != EOL; ptr = links[ptr]){
      s = list[ptr];
      if(eyerec[s] != 0){
	 deallocate_eye(eyerec[s]);
	 }
      }
  
  
   for(ptr = eyelist; ptr != EOL; ptr = links[ptr]){
      s = list[ptr];
      g = board[s];

	/* is this spot part of an eye? */
      if(!can_be_eye(s,g))continue;
      if(eyerec[s] != 0)continue;  /* already did this eye */

      c = grcolor[lgr[s]];

      if(ld[s] == 8)
         	evalonepteye(s,c);

      else if(ld[s] == 0)
         	evaldeadgroupeye(s,lgr[s],1-c);

      else if(lnbn[s] == 1){
	 	sopen = list[nblbp[s]];

         	if(lnbn[sopen] == 1 && ld[s] == 7)	/* two point eye */
            		eval2pointeye(s,sopen,c);

		else if(lnbn[sopen] == 2 && lnbf[sopen][1-c] == 0)  /* line eye */
			eval_line_eye(s,sopen,c);

         	else 		/* more than 2 point eye */
	    		evalbigeye(s,sopen,c);
         	}
      else if(lnbn[s] == 2 && ld[s] != NOLD)evalopenlineeye(s,c);
      }
   killist(&eyelist);

   evalcornereyes();
   }


/* evaluate line eyes that are open at both ends 
 * start with point s.  c is friendly color */
/* evlol is value (opp moves first) of open line eye on edge 
 * blocked at both ends 
 */

int evlol[] = { 0,0,0,0,0,8,8,8,16,16 };

void evalopenlineeye(int s,int c){
	int length, points = EOL; 
	     /* length is number of points.  Points are places with two
              * liberties and only friendly neighbors
              */
	int end1,end2;  /* ends are points after last point */
	int v1,v2;      /* v1,v2 will be the vital points */
	int rn,sn,ptr,corner=NOSQUARE;
	addlist(s,&points);
	length = 1;
	end1 = list[nblbp[s]];
	end2 = list[links[nblbp[s]]];
	v1 = end1;
	v2 = end2;
	while(lnbn[end1] <= 2 && lnbf[end1][1-c] == 0){
		if(lnbn[end1] == 1){
			killist(&points);
			return;  /* end not open */
			}
		addlist(end1,&points);
		++length;
		v1 = end1;
		if(edge[end1] == 0)corner = end1;
		if(end1 == end2)break;
		sn = list[nblbp[end1]];
		if(inlist(sn,&points))
			sn = list[links[nblbp[end1]]];
		end1 = sn;
		}
	if(end1 != end2)
  	    while(lnbn[end2] <= 2 && lnbf[end2][1-c] == 0){
		if(lnbn[end2] == 1){
			killist(&points);
			return;  /* end not open */
			}
		addlist(end2,&points);
		++length;
		v2 = end2;
		if(edge[end2] == 0)corner = end2;
		sn = list[nblbp[end2]];
		if(inlist(sn,&points))
			sn = list[links[nblbp[end2]]];
		end2 = sn;
		}

	rn = gtflist(&eyefreelist);
	eyeptr[rn] = points;
	for(ptr = points; ptr != EOL; ptr = links[ptr]){
		eyerec[list[ptr]] = rn;
		}



	if(length == 1 && end1+end2-s-s != 0 && lnbf[end1][c] != 0 &&
	   lnbf[end1][1-c] == 0 && lnbf[end2][c] != 0 && lnbf[end2][1-c] == 0){
		/* corner eye in middle of board */
		eyetype[rn] = OPENLINEEYE;
		eyeval[rn] = 0;
		eyepot[rn] = 8;
		eyemin[rn] = 0;
		sn = end1+end2-s;
		if(ld[sn] == 6)eyepot[rn] = 4;  /* so don't double count two
						   facing each other */
		addlist(sn,&eyevital[rn]);
		addlist(rn,&eyevitrec[sn]);
		return;
		}

	if(length == 2 && (abs(end1-end2) == 1 || abs(end1-end2) == boardsize) &&
	   lnbf[end1][1-c] == 0 && lnbf[end2][1-c] == 0 &&
	   (lnbf[end1][c] != 0 || lnbf[end2][c] != 0)){
		eyetype[rn] = OPENLINEEYE;
		eyeval[rn] = 0;
		eyepot[rn] = 8;
		eyemin[rn] = 0;
		addlist(end1,&eyevital[rn]);
		addlist(rn,&eyevitrec[end1]);
		addlist(end2,&eyevital[rn]);
		addlist(rn,&eyevitrec[end2]);
		return;
		}

	if(length == 3 && end1 == end2){
		eyetype[rn] = OPENLINEEYE;
		eyeval[rn] = 0;
		eyepot[rn] = 8;
		eyemin[rn] = 0;
		addlist(end1,&eyevital[rn]);
		addlist(rn,&eyevitrec[end1]);
		return;
		}

	if(end1 == end2 && length == 4){  /* block 4 eye */
		eyetype[rn] = FOURPTBLOCKEYE;
		eyepot[rn] = 8;
		eyeval[rn] = 8;
		eyemin[rn] = 8;
		return;
		}

	if(!inlist(end1,&points) && lnbf[end1][1-c] == 0){
		++length;
		v1 = end1;
		}
	if(!inlist(end2,&points) && lnbf[end2][1-c] == 0 ){
		++length;
		v2 = end2;
		}
			
	if(length > 8)length = 8;
	eyetype[rn] = OPENLINEEYE;
	eyeval[rn] = evlol[length];
	eyepot[rn] = evlol[length+1];
	eyemin[rn] = evlol[length-1];
	if(eyepot[rn] != eyeval[rn] || length >= 6){
		addlist(v1,&eyevital[rn]);
		addlist(v2,&eyevital[rn]);
		addlist(rn,&eyevitrec[v1]);
		addlist(rn,&eyevitrec[v2]);
		if(corner != NOSQUARE){
			addlist(corner,&eyevital[rn]);
			addlist(rn,&eyevitrec[corner]);
			}
		}
	}



void addeyerec(int s){
	int rn,ptr;
	rn = eyerec[s];
	for(ptr = eyeptr[rn]; ptr != EOL; ptr = links[ptr])
			addlist(list[ptr],&eyelist);
	}



/* find all of the eyelist entries for this move.
 * these are potentially changed eyes.  they must be points with
 * 3 or 4 friendly neighbors.
 */


void findeyelist(void){
   int i,s,j,sn,k,sn2,ldtmp,ldtm2,tmplist=EOL,ptr;
   addlist(0,&eyelist);  /* reeval eyes in corners */
   addlist(boardsize-1,&eyelist);
   addlist(boardsquare-1,&eyelist);
   addlist(boardsquare-boardsize,&eyelist);
   cpylist(eyelist,&tmplist);
   for(ptr = tmplist; ptr != EOL; ptr = links[ptr])
	if(eyerec[list[ptr]] != 0)addeyerec(list[ptr]);
        else mrglist(nblbp[list[ptr]],&eyelist);
   killist(&tmplist);
   for(i = 0; i < pclsnext; ++i){ 
      s = pcls[i];
      if(ld[s] == 0 &&
         (gralive[board[s]] == DEAD || grthreatened[board[s]])){ 
         addlist(s,&eyelist); 
         }
      else if(ld[s] > 5){ 
         addlist(s,&eyelist);
         }
      if(eyesdiags[s] != EOL)mrglist(eyesdiags[s],&eyelist);
      if(eyerec[s] != 0)addeyerec(s);
      j = fdir[s];
      for(ldtmp = ldir[j]; j != ldtmp; ++j){
         sn = s + nbr[j]; 
         if(eyerec[sn] != 0)addeyerec(sn);
	 if(ld[sn] > 5)addlist(sn,&eyelist);
	 if(eyesdiags[sn] != EOL)mrglist(eyesdiags[sn],&eyelist);
         k = fdir[sn];
         for(ldtm2 = ldir[k]; k != ldtm2; ++k){ 
            sn2 = sn + nbr[k];
            if(eyerec[sn2] != 0)addeyerec(sn2);
            } 
         }
      }
   }





/* return TRUE if this spot can be part of an eye
 * currently, one point surrounded on 4 sides by friendly stones,
 * or a dead or threatened grouop,
 * or a point surrounded on 3 sides by friendly stones
 * or a point with ld == 6
 */

int can_be_eye(int s,int g){
      	return(g == NOGROUP && ld[s] >= 6 && ld[s] != NEUTRALLD ||
         	g != NOGROUP && (gralive[g] == DEAD || grthreatened[g]));
	}

int opt1[] = {0,0,0,8,0,8,8,8};  /* eyepot */
int opt2[] = {0,0,0,0,0,0,8,8};  /* eyeval */
int opt3[] = {0,0,0,0,0,0,0,8};  /* eyemin */

/* evaluate a one point eye */

void evalonepteye(int s,int c){
	int rn,count;
	rn = gtflist(&eyefreelist);

	eyerec[s] = rn;
	eyetype[rn] = ONEPOINTEYE;
	addlist(s,&eyeptr[rn]);
        count = getcount(s,c,rn,TRUE,NUMGROUPS+NUMCONNS+s);
	eyepot[rn] = opt1[count];
	eyeval[rn] = opt2[count];
	eyemin[rn] = opt3[count];
	}


int ed1[] = { 1,1,-1,-1};
int ed2[] = { 1,-1,1,-1};

void evalcornereyes(void){
	int s,dir1,dir2,rn,cn,rf,lf,c;
	for(cn = 0; cn < 4; cn ++){
		s = 0;
		rf = lf = FALSE;
		if(cn == 1)s = boardsize-1;
		if(cn == 2)s = boardsquare - boardsize;
		if(cn == 3)s = boardsquare - 1;
		dir1 = ed1[cn]*boardsize;
		dir2 = ed2[cn];
		if(eyerec[s] != 0 || eyerec[s+dir1] != 0 ||
		   eyerec[s+dir2] != 0)continue;  /* already is an eye */
		if(board[s+dir1+dir2] != NOGROUP){
		   c = grcolor[board[s+dir1+dir2]];
		   if(ld[s+dir1] >= 4 && board[s+dir2] == NOGROUP ||
		      ld[s+dir2] >= 4 && board[s+dir1] == NOGROUP){
			   rn = gtflist(&eyefreelist);
			   eyerec[s] = rn;
			   addlist(s,&eyeptr[rn]);
			   if(ld[s+dir1] >= 4){
				   eyerec[s+dir1] = rn;
				   addlist(s+dir1,&eyeptr[rn]);
				   addlist(s+dir2,&eyevital[rn]);
				   addlist(rn,&eyevitrec[s+dir2]);
				   if(board[s+dir1+dir1] == NOGROUP &&
				      lnbf[s+dir1+dir1][1-c] == 0 &&
				      (grcolor[board[s+dir1+dir1+dir2]] == c ||
				       board[s+dir1+dir1+dir2] == NOGROUP &&
				       lnbf[s+dir1+dir1+dir2][1-c] == 0))
					   rf = TRUE;
				   }
			   if(ld[s+dir2] >= 4){
				   eyerec[s+dir2] = rn;
				   addlist(s+dir2,&eyeptr[rn]);
				   addlist(s+dir1,&eyevital[rn]);
				   addlist(rn,&eyevitrec[s+dir1]);
				   if(board[s+dir2+dir2] == NOGROUP &&
				      lnbf[s+dir2+dir2][1-c] == 0 &&
				      (grcolor[board[s+dir2+dir2+dir1]] == c ||
				       board[s+dir2+dir2+dir1] == NOGROUP &&
				       lnbf[s+dir2+dir2+dir1][1-c] == 0))
					   lf = TRUE;
				   }
			   eyetype[rn] = CORNEREYE;
			   eyepot[rn] = 8;
			   eyeval[rn] = 0;
			   if(rf && lf)eyeval[rn] = 8;
			   eyemin[rn] = 0;
			   }
		   }
		}
	return;

   }


/* deallocate_eye gets rid of an eye record.  All eye records for eyes
 * that changed are deallocated at the start of fixli and reconstructed later
 */

void deallocate_eye(int rn){
	int ptr,i,ldtmp;

	for(ptr = eyeptr[rn]; ptr != EOL; ptr = links[ptr]){
		eyerec[list[ptr]] = 0;
		i = fdir[list[ptr]];
            	for(ldtmp = ldiag[i]; i < ldtmp; ++i)
               		dellist(list[ptr],&eyesdiags[list[ptr]+diags[i]]);
		if(grldr[NUMGROUPS+NUMCONNS+list[ptr]] != EOL)
			kill_ldrflags(NUMGROUPS+NUMCONNS+list[ptr]);
		}
	killist(&eyeptr[rn]);
	for(ptr = eyevital[rn]; ptr != EOL; ptr = links[ptr])
		dellist(rn,&eyevitrec[list[ptr]]);
	killist(&eyevital[rn]);
	eyetype[rn] = NOEYE;
	adflist(rn,&eyefreelist);
	eyeval[rn] = eyepot[rn] = eyemin[rn] = 0;
	}



/* getcount examines the diagonals of an eye and returns a count
 * 7 eye can never be taken away (3 or 4 corners controlled)
 * 6 0 opponent corner, 2 controlled (cant stop from making an eye)
 * 5 0 opponent corner, 1 controlled (get eye if move first)
 *	(adds three vital points)
 * 4 0 opponent corner, 0 controlled (cant get eye)
 * 3 1 opponent corner, 2 controlled (get eye if you move first)
 *     (adds vital point)
 * 2 1 opponent corner, 1 controlled (cant get eye)
 * 1 1 opponent corner, 0 controlled (cant get eye)
 * 0 can never be an eye (2 opponent corners)
 *
 * rn is the eye record to add vital points to.
 * if(vital) then add vital points to the eye record
 */
int vtp[30];

int getcount(int s,int c,int rn,int vital,int ldrno){
   int count,haveatari,countlist,gcount,bcount,ptr2,sn,deadgroup;
   int vtc=0,i,ldtmp,sn2,flag,ptr,j,ldtm2;

	i = fdir[s];
   for(ldtmp = ldiag[i]; i < ldtmp; ++i)
        	addlist(s,&eyesdiags[s+diags[i]]);

   count = 0;
   haveatari = FALSE;
   countlist = EOL;
   gcount = 0;
   bcount = 0;
   if(edge[s] == 1){
      gcount = 1;
      bcount = 1;
      }
   else if(edge[s] == 0){
      gcount = 2;
      bcount = 1;
      }
   i = fdir[s];
   for(ldtmp = ldiag[i]; i < ldtmp; ++i){
      sn = s + diags[i];
      if(board[sn] != NOGROUP){
         deadgroup = gralive[lgr[sn]] == DEAD;
         if(grcolor[lgr[sn]] == c){
            if(deadgroup)++bcount;
            else ++gcount;
            }
         else if(deadgroup)++gcount;
         else{
            if(grlibs[lgr[sn]] == 1 || grthreatened[lgr[sn]]){
		haveatari = TRUE;
		for(ptr = grlbp[lgr[sn]]; ptr != EOL; ptr = links[ptr]){
			vtp[vtc++] = list[ptr];
			if(vtc > 18)break;
			}
		}
            ++bcount;
            }
         }
      else if(lnbn[sn] > 2){
	      flag = FALSE;
	      for(ptr = nblbp[sn]; ptr != EOL; ptr = links[ptr])
		      if(lnbn[list[ptr]] > 2){
			      flag = TRUE;
			      break;
			      }
	      if(!flag)addlist(sn,&countlist);
	      else
		      vtp[vtc++] = sn;
	      continue;
	      }
      else if(lnbn[sn] < 2 && lnbf[sn][1-c] == 0){
	      j = fdir[sn];
	      flag = FALSE;
	      for(ldtm2 = ldir[j]; j != ldtm2; ++j){
		      sn2 = sn + nbr[j];
		      if(board[sn2] == NOGROUP)continue;
		      if(grlibs[board[sn2]] == 1){
			      adflist(sn,&countlist);
			      flag = TRUE;
			      break;
			      }
		      }
	      if(!flag)gcount++;
	      }
      else {
	      adflist(sn,&countlist);
	      }
      }
   if(bcount < 2 && gcount < 3 && (bcount > 0 || gcount < 2)){
      ptr2 = countlist;
      while(ptr2 != EOL && gcount + haveatari < 3){
         if(cntplyhere(list[ptr2],c,ldrno))++gcount;
	 else
		vtp[vtc++] = list[ptr2];
         ptr2 = links[ptr2];
         }
      if(bcount+gcount < 4 && haveatari)++gcount;
      }
   killist(&countlist);

   if(gcount >= 3)count = 7;
   else if(gcount == 2 && bcount == 2 && haveatari)count = 3;
   else if(bcount >= 2)count = 0;
   else count = 4 + gcount - 3*bcount;
   if(vital && (count == 3 || count == 5))
	for(i = 0; i < vtc; ++i){
		addlist(vtp[i],&eyevital[rn]);
		addlist(rn,&eyevitrec[vtp[i]]);
		}
   return(count);
   }  

int diffs4[5][3];
int diffs4i[7][3] =
{
{  1,18, 1 },  /* square */
{  1, 1,18 },  /* pyramid */
{ 18, 1,19 },  /* pyramid */ 
{ 18, 1, 1 },  /* pyramid */
{ 19, 1,18 },  /* pyramid */
{  1, 1, 1 },  /* straight line */
{ 19,19,19 },  /* straight line */
}; 

int diffs5[9][4];
int diffs5i[9][4] =
{
{  1, 1,17, 1 },  /* 8 bulky 5 shapes */
{  1,18, 1,19 },
{  1,17, 1, 1 },
{ 19, 1,18, 1 },
{  1,18, 1, 1 },
{  1,18, 1,18 },
{  1, 1,18, 1 },
{ 18, 1,18, 1 }, 
{ 18, 1, 1,18 }, };  /* plus */

int diffs6[4][5];
int diffs6i[4][5] =
{
{ 18, 1, 1,17, 1 },
{  1,18, 1, 1,18 },
{  1,17, 1, 1,18 },
{ 18, 1, 1,18, 1 }, };


/* deadshape takes a group and eye record number and sets the eyemin,
 * val, max, and vital points for the shape of the group.  the group is
 * between 4 and 6 stones inclusive.
 */

void deadshape(int g,int rn){
   int pointlist,size,count; 
   int ptr,i,j,diffs[5],ldtmp;
   eyeval[rn] = eyepot[rn] = eyemin[rn] = 8;
   size = grsize[g];
   pointlist = EOL;
   for(ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr])
      addlist(mvs[ptr],&pointlist);

   i = 0;
   for(ptr = pointlist; links[ptr] != EOL; ptr = links[ptr]){
      diffs[i] = list[links[ptr]] - list[ptr];
      ++i;
      }
   killist(&pointlist);
   if(size == 4){
      for(j = 0; j < 3; ++j)
         if(diffs4[0][j] != diffs[j])break;
         else if(j == 2)return;	/* 4 in square */
      for(i = 1; i < 5; ++i)
         for(j = 0; j < 3; ++j)
            if(diffs4[i][j] != diffs[j])break;
            else if(j == 2)return;  /* 4 in pyramid */
      eyeval[rn] = eyemin[rn] = eyepot[rn] = 16; /* 4 in line */
      for(i = 5; i < 7; ++i)
	 for(j = 0; j < 3; ++j)
	    // sm: below, the original code accessed the 'diffs4'
	    // array with first index up to 7, whereas it's declared
	    // to only go to 5.. the 'diffs4i' array is declared to
	    // go up to 7, so I think that is what was intended (unless
	    // the programmer was relying on subtle column-major
	    // ordering and some invariants about i vs j...)
	    // I confirmed this doesn't change answer for "50 9"
            //sm: if(diffs4[i][j] != diffs[j])break;
	    if(diffs4i[i][j] != diffs[j])break;
	    //       ^
            else if(j == 2)return;  /* 4 in straight line */
      for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
	      i = fdir[list[ptr]];
	      count = 0;
	      for(ldtmp = ldir[i]; i < ldtmp; ++i)
		      if(board[list[ptr]+nbr[i]] == g)++count;
	      if(count == 2){
		      eyemin[rn] = eyeval[rn] = 8;
		      addlist(list[ptr],&eyevital[rn]);
		      addlist(rn,&eyevitrec[list[ptr]]);
		      return;  /* can make bulky 5 */
		      }
	      }
      return;  
      }
   if(size == 5){
      for(i = 0; i < 9; ++i)
         for(j = 0; j < 4; ++j)
            if(diffs5[i][j] != diffs[j])break;
            else if(j == 3)return;  /* bulky 5 or plus */
      eyeval[rn] = eyemin[rn] = eyepot[rn] = 16;
      return;  /* other 5 shapes */
      }
   for(i = 0; i < 4; ++i)
      for(j = 0; j < 5; ++j)
         if(diffs6[i][j] != diffs[j])break;
         else if(j == 4)return;  /* rabbity 6 */
   eyeval[rn] = eyemin[rn] = eyepot[rn] = 16;
   return;  /* other 6 shapes */
   }
   



/* evaldeadgroupeye figures out the eyes for the dead group g 
 * s is the spot for the ladder number. c is the color of the surrounding 
 * stones, not the dead group 
 * if dead group is one stone, treat like eye surrounded on 3 sides.
 */

void evaldeadgroupeye(int s,int g,int c){
   int s1,s2,e1,e2;
   int rn,ldrno;
   int ptr;

   if(eyerec[mvs[grpieces[g]]] != 0)return;  /* already did this eye */

   rn = gtflist(&eyefreelist);

   for(ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr]){
      	eyerec[mvs[ptr]] = rn;
      	addlist(mvs[ptr],&eyeptr[rn]);
      	}
   ldrno = NUMGROUPS+NUMCONNS+mvs[grpieces[g]];
   if(gralive[g] == DEAD)
	eyetype[rn] = DEADEYE;
   else
	eyetype[rn] = THRTEYE;

   if(grsize[g] == 1)
      	evaloneptdeadeye(s,g,c,rn,ldrno);

   else if(grsize[g] == 2){
      	s1 = mvs[grpieces[g]];
      	s2 = mvs[mvnext[grpieces[g]]];
      	e1 = c8[getcount(s1,c,rn,FALSE,ldrno)];
      	e2 = c8[getcount(s2,c,rn,FALSE,ldrno)];
      	if(e1 + e2 == 16)
		eyepot[rn] = eyeval[rn] = eyemin[rn] = 8;
	else if(e1 + e2 == 12){
		eyepot[rn] = 8;
		eyeval[rn] = 4;
		eyemin[rn] = 4;
		if(e1 == 4)getcount(s1,c,rn,TRUE,ldrno);  /* fix vital point */
		else getcount(s2,c,rn,TRUE,ldrno);
		}
      	}
   else if(grsize[g] == 3){
	eyepot[rn] = eyeval[rn] = eyemin[rn] = 8;
      	}
   else if(grsize[g] < 7){
        deadshape(g,rn);
        }
   else{
      eyepot[rn] = eyeval[rn] = eyemin[rn] = 16;
      }
   }


int othr1[] = {0,0,0,0,0,0,8,8}; /* eyepot (can't move first after threat) */
int othr2[] = {0,0,0,0,0,0,8,8}; /* eyeval */
int othr3[] = {0,0,0,0,0,0,0,8}; /* eyemin */

int odead1[] = {0,0,0,8,0,8,8,8}; /* eyepot */
int odead2[] = {0,0,0,0,0,0,8,8}; /* eyeval */
int odead3[] = {0,0,0,0,0,0,0,8}; /* eyemin */

/* evaluate the eye made by a single dead stone at s
 * g is the dead group
 * c is the color of the side that gets the eye.
 */


void evaloneptdeadeye(int s,int g,int c,int rn,int ldrno){
      int nbflag, nbgr, ptr2, sopen, eyespots, numeyespots, j;
      int numopenspots, ldtm2, sn2, count, flag, numenemyspots;

      nbflag = FALSE;
      for(ptr2 = grnbp[g]; ptr2 != EOL; ptr2 = links[ptr2])
         if(grlibs[list[ptr2]] == 1){
            nbflag = TRUE;
            nbgr = list[ptr2];
            break;
            }
      if(nbflag){	/* nbr in atari */
         if(list[grlbp[nbgr]] == kosquare){
	    return;
            }
         }
      if(grlibs[g] == 1){
         sopen = list[grlbp[g]];
         eyespots = EOL;
         numeyespots = 0;
	 numopenspots = 0;
	 numenemyspots = 0;
         j = fdir[sopen];
         for(ldtm2 = ldir[j]; j != ldtm2; ++j){
            sn2 = sopen + nbr[j];
            if(lnbn[sn2] == 1 && 
               (ld[sn2] != 0 && ld[sn2] != NEUTRALLD && grcolor[lgr[sn2]] == c ||
                ld[sn2] == 0 && grcolor[lgr[sn2]] != c && grsize[lgr[sn2]]
                == 1 && grlibs[lgr[sn2]] == 1)){
		if(eyerec[sn2] != 0 && eyerec[sn2] != rn)
			deallocate_eye(eyerec[sn2]);
		addlist(sn2,&eyeptr[rn]);
		eyerec[sn2] = rn;
               adflist(sn2,&eyespots);
               numeyespots++;
               }
            else if(grcolor[board[sn2]] == 1-c)++numenemyspots;
	    else if(board[sn2] == NOGROUP)++numopenspots;
            }
         if(numeyespots > 1){
            evalmanyeyespots(rn,sopen,c,numopenspots,numenemyspots,ldrno);
            }
         else{	/* ony one eyespot */
	    if(gralive[g] == DEAD){
		count = getcount(s,c,rn,TRUE,ldrno);
		eyeval[rn] = odead2[count];
		eyemin[rn] = odead3[count];
            	}

	    mvcolor[msptr] = c;
	    mvs[msptr] = sopen;
	    flag = lupdate(msptr);
	    if(flag){
		    upldrflags(msptr,ldrno);
		    ++msptr;
		    count = getcount(s,c,rn,FALSE,ldrno);
		    --msptr;
		    }
	    else count = 0;
	    if(gralive[g] != DEAD){
		    eyepot[rn] = othr1[count];
		    eyeval[rn] = othr2[count];
		    eyemin[rn] = othr3[count];
		    addlist(sopen,&eyevital[rn]);
		    addlist(rn,&eyevitrec[sopen]);
		    }
	    else {
		    eyepot[rn] = odead2[count];
		    if(eyepot[rn] != eyeval[rn]){
			    addlist(sopen,&eyevital[rn]);
			    addlist(rn,&eyevitrec[sopen]);
			    }
		    }
	    ldndate(msptr);
            }
         killist(&eyespots);
         }
      else {	/* one stone group w more than one liberty */
         if(gralive[g] == DEAD){
		count = getcount(s,c,rn,TRUE,ldrno);
		eyepot[rn] = odead1[count];
		eyeval[rn] = odead2[count];
		eyemin[rn] = odead3[count];
		}
         else if(edge[mvs[grpieces[g]]] <= 2){  /* special case for stone on 2 line */
		eyepot[rn] = 8;
		eyeval[rn] = 8;
		eyemin[rn] = 0;
		}
	 else {
		count = getcount(s,c,rn,FALSE,ldrno);
		eyepot[rn] = othr1[count];
		eyeval[rn] = othr2[count];
		eyemin[rn] = othr3[count];
		}
         } 
   }

/* eval an eye with several points surrounded on 3 sides opening into
 * a common point.  Sopen is the common point.  rn is the eye record
 * number.  c is color of side with eye.  One or more of the points
 * could be a single stone group.  numopenspots is the number of
 * spots next to sopen which are empty (and not surrounded on 3 sides)
 * numenemyspots is the number of spots next to sopen that have
 * uncaptured enemy stones on them.
 */


void evalmanyeyespots(int rn,int sopen,int c,int numopenspots,int numenemyspots,int ldrno){
	int sn2,eye_pot, count,opp_can_fill,flag,ptr2;

	/* first see what happens if opponent moves in sopen */

        if(numopenspots == 0){
		mvcolor[msptr] = 1-c;
		mvs[msptr] = sopen;
		opp_can_fill = lupdate(msptr);
		ldndate(msptr);
		}
	else opp_can_fill = TRUE;
	if(opp_can_fill){
		addlist(sopen,&eyevital[rn]);
		addlist(rn,&eyevitrec[sopen]);
		}
	/* now see how many eyes we can make */

	mvcolor[msptr] = c;
	mvs[msptr] = sopen;
	flag = lupdate(msptr);
	upldrflags(msptr,ldrno);
        ++msptr;
	eye_pot = 0;
	for(ptr2 = eyeptr[rn]; ptr2 != EOL; ptr2 = links[ptr2]){
	   sn2 = list[ptr2];               
	   count = getcount(sn2,c,rn,FALSE,ldrno);
	   eye_pot += cpot[count];
	   }
	--msptr;
	ldndate(msptr);

	eyepot[rn] = eye_pot;

        if(!opp_can_fill){
		eyeval[rn] = eye_pot;
		eyemin[rn] = eye_pot;
		}
	
	else if(eye_pot >= 16){		/* can make two eyes */
		if(numopenspots > 1 || numenemyspots != 0)
			eyeval[rn] = 0;
		else
			eyeval[rn] = 8;
		if(numopenspots == 0 && numenemyspots == 0)
			eyemin[rn] = eyeval[rn];
		else eyemin[rn] = 0;
	      	}
	else if(ld[sopen] == 6 && eye_pot > 0){	/* enclosed 3 space eye */
		eyeval[rn] = 8;
		eyemin[rn] = 0;
	      	}
	}

/* 0,0 = two corners controlled by opponent
 * 1,4 = one opponent, one open
 * 2,8 = both open
 * 3,12 = one or both controlled by self
 */

int ind1[] = { 0,0,0,1,0,0,2,3};
int ind2[] = { 0,0,0,4,0,0,8,12};	

int tpt1[] = {  0,0,8,8,
		0,8,8,8,
		8,8,8,8,
		8,8,8,8 };

int tpt2[] = {  0,0,0,0,
		0,0,8,4,
		0,8,8,8,
		0,4,8,8 };

int tpt3[] = {  0,0,0,0,
		0,0,0,0,
		0,0,8,8,
		0,0,8,8 };

/* evaluate 2 point completely surrounded eye at s and sopen */


void eval2pointeye(int s,int sopen,int c){
	int sn,tmplist,ptr2,gnbr,rn,count1,count2,index,ldrno;
	rn = gtflist(&eyefreelist);

	eyerec[s] = rn;
	eyerec[sopen] = rn;
	eyetype[rn] = TWOPOINTEYE;
	addlist(s,&eyeptr[rn]);
	addlist(sopen,&eyeptr[rn]);
        ldrno = NUMGROUPS+NUMCONNS+s;
	count1 = getcount(s,c,rn,FALSE,ldrno);
	count2 = getcount(sopen,c,rn,FALSE,ldrno);
	index = ind1[count1] + ind2[count2];
	eyepot[rn] = tpt1[index];
	eyeval[rn] = tpt2[index];
	eyemin[rn] = tpt3[index];
        if(eyeval[rn] == 0 && eyepot[rn] == 8){
		count1 = getcount(s,c,rn,TRUE,ldrno);
		count2 = getcount(sopen,c,rn,TRUE,ldrno);
		}
	if(ind1[count1] == 0 && (ind2[count2] == 12 || ind2[count2] == 8)){
		addlist(s,&eyevital[rn]);
		addlist(rn,&eyevitrec[s]);
		}
	if((ind1[count1] == 2 || ind1[count1] == 3) && ind2[count2] == 0){
		addlist(sopen,&eyevital[rn]);
		addlist(rn,&eyevitrec[sopen]);
		}
	if(eyeval[rn] == 4){	/* need more reading */
		eyeval[rn] = 8;
	   	sn = sopen;
	   	if(ind1[count2] == 3){
	     		sn = s;
	     		}
	   	mvs[msptr] = sn;
	   	mvcolor[msptr] = 1-c;
	   	lupdate(msptr);
	        upldrflags(msptr,ldrno);
	   	++msptr;
				/* put down throw in stone */
	   	tmplist = EOL;
	   	cpylist(grnbp[board[sn]],&tmplist);
	   	for(ptr2 = tmplist; ptr2 != EOL; ptr2 = links[ptr2]){
	      		gnbr = list[ptr2];
	      		if(grlibs[gnbr] == 1){   /* throwin is atari? */
	         		if(list[grlbp[gnbr]] == list[grlbp[board[sn]]]){
	            			eyeval[rn] = 0;
					addlist(sn,&eyevital[rn]);  
					addlist(rn,&eyevitrec[sn]);
	            			break;
	            			}
	         		else {
	            			mvs[msptr] = list[grlbp[gnbr]];
	            			mvcolor[msptr] = c;
	            			lupdate(msptr);
					upldrflags(msptr,ldrno);
	            			if(grlibs[board[mvs[msptr]]] == 1){
	               				eyeval[rn] = 0;
						addlist(sn,&eyevital[rn]);
						addlist(rn,&eyevitrec[sn]);
	               				}
	            			ldndate(msptr);
	            			} 
	         		}
	      		}
	   	killist(&tmplist);
	   	--msptr;
	   	ldndate(msptr);
		}
	}


/* values for line eyes closed at both ends by number of spaces in line */

int lineval[] = { 0,8,8, 8,16,16 };  /* he moves first */
int linemin[] = { 0,8,8, 8, 8,16 };  /* he gets two moves */

/* evaluate linear eyes without enemy stones. 2 space eye closed
 * at both ends is treated as a special case elsewhere. 
 */

void eval_line_eye(int s,int sopen,int c){
	int rn,length,sn,sold,ptr,cn,vlength,potlength,cn2,corner=NOSQUARE;
        int ldrno;

   	rn = gtflist(&eyefreelist);

   	eyerec[s] = rn;
   	eyetype[rn] = LINEEYE;
   	addlist(s,&eyeptr[rn]);
        ldrno = NUMGROUPS+NUMCONNS+s;


	sn = sopen;	/* find end of line */
	sold = s;
	length = 1;
	do {
		addlist(sn,&eyeptr[rn]);
		eyerec[sn] = rn;
		if(edge[sn] == 0)corner = sn;
		for(ptr = nblbp[sn]; ptr != EOL; ptr = links[ptr])
			if(list[ptr] != sold){
				sold = sn;
				sn = list[ptr];
				break;
				}
		++length;
		} while(lnbn[sn] == 2 && lnbf[sn][1-c] == 0);

	/* points from s to sold inclusive are included in eye and length */

	if(lnbn[sn] == 1 && lnbf[sn][1-c] == 0){  /* closed off end */
		addlist(sn,&eyeptr[rn]);
		eyerec[sn] = rn;
		sold = sn;
		length++;  /* measure full length */

		vlength = length;
		potlength = length;
		cn = getcount(sn,c,rn,TRUE,ldrno);
		if(cn == 3)length--;
		else if(cn < 6){
			length -= 2;
			potlength -= 1;
			addlist(sn,&eyevital[rn]);
			addlist(rn,&eyevitrec[sn]);
			}
		cn2 = getcount(s,c,rn,TRUE,ldrno);
		if(cn2 == 3)length--;
		else if(cn2 < 6){
			potlength -= 1;
			length -= 2;
			addlist(s,&eyevital[rn]);
			addlist(rn,&eyevitrec[s]);
			}
		if(cn < 6 && cn2 < 6)potlength--;
		if(length == 3 && vlength == 3){
			addlist(sopen,&eyevital[rn]);
			addlist(rn,&eyevitrec[sopen]);
			potlength++;
			}
		if(length < 0)length = 0;
		if(length > 5)length = 5;
		if(potlength < 0)potlength = 0;
		if(potlength > 5)potlength = 5;
		eyeval[rn] = lineval[length];
		eyepot[rn] = lineval[potlength];
		eyemin[rn] = linemin[length];
		if(eyeval[rn] != eyemin[rn]){  /* check if can atari inside */
			if(grlibs[lgr[sopen]] == 2){
				eyeval[rn] = eyemin[rn];
				if(eyeval[rn] != eyepot[rn]){
					addlist(sopen,&eyevital[rn]);
					addlist(rn,&eyevitrec[sopen]);
					}
				}
			if(grlibs[lgr[sold]] == 2){
				eyeval[rn] = eyemin[rn];
				if(eyeval[rn] != eyepot[rn]){
					addlist(sold,&eyevital[rn]);
					addlist(rn,&eyevitrec[sold]);
					}
				}
			}
		if(eyeval[rn] != eyepot[rn] && corner != NOSQUARE){
			addlist(corner,&eyevital[rn]);
			addlist(rn,&eyevitrec[corner]);
			}
		}
	
	else {  /* open ended line. s to sold in line.  sn is past end */
		/* for open ended line, length is best potential line length */

		if(lnbf[sn][1-c] != 0 && lnbf[sn][c] == 0){ 
			              /* enemy can hane or throw in */
			length--;
			addlist(sold,&eyevital[rn]);
			addlist(rn,&eyevitrec[sold]);
			}
		if(lnbn[sn] == 1 && lnbf[sn][1-c] >= 1 && lnbf[sn][c] != 0){ /*enemy can push in */
			addlist(sn,&eyevital[rn]);
			addlist(rn,&eyevitrec[sn]);
			}
		else if(lnbf[sn][1-c] == 0){
			addlist(sn,&eyevital[rn]);
			addlist(rn,&eyevitrec[sn]);
			}
		if(lnbf[sn][1-c] == 0)
			for(ptr = nblbp[sn]; ptr != EOL; ptr = links[ptr])
				if(ld[list[ptr]] == NEUTRALLD){
					addlist(list[ptr],&eyevital[rn]);
					addlist(rn,&eyevitrec[list[ptr]]);
					}

		cn = getcount(s,c,rn,TRUE,ldrno);
		if(cn == 3 || cn == 5)length--;
		else if(cn < 6){
			length -= 2;
			addlist(s,&eyevital[rn]);
			addlist(rn,&eyevitrec[s]);
			if(edge[list[nblbp[s]]] == 0 && 
			   grlibs[board[s+s-list[nblbp[s]]]] == 1)length--;
			}
		if(length > 5)length = 5;
		if(length < 0)length = 0;
		eyepot[rn] = lineval[length];
		if(length < 1)length = 1;
		eyeval[rn] = lineval[length-1];
		if(length < 2)length = 2;
		eyemin[rn] = lineval[length-2];
		if(eyeval[rn] != eyepot[rn]){
			if(corner != NOSQUARE){
				addlist(corner,&eyevital[rn]);
				addlist(rn,&eyevitrec[corner]);
				}
			addlist(sold,&eyevital[rn]);
			addlist(rn,&eyevitrec[sold]);
			}
		}

	}

int onesp1[] = { 0,0,0,4,0,4,8,8 }; /* potential for one eyespot */

/* figure out how much a corner opening into a big area is worth */


void evalbigeye(int s,int sopen,int c){
	int i,ldtmp,sn,j,sn2,ldtm2,flag,count,numeyespots,ldrno;
	int tmpli,ptr,rn,numopenspots,numenemyspots;

	if(eyerec[s] != 0)return;  /* already evaluated */

   	rn = gtflist(&eyefreelist);

   	eyerec[s] = rn;
   	eyetype[rn] = BIGEYE;
   	addlist(s,&eyeptr[rn]);
        ldrno = NUMGROUPS+NUMCONNS+s;


	/* find other eyespots sharing same sopen */

	numeyespots = numopenspots = numenemyspots = 0;
	j = fdir[sopen];
	for(ldtm2 = ldir[j]; j != ldtm2; ++j){
	   sn2 = sopen + nbr[j];
	   if(lnbn[sn2] == 1 && 
	      (ld[sn2] > 1 && grcolor[lgr[sn2]] == c ||
	       ld[sn2] == 0 && grcolor[lgr[sn2]] != c && grsize[lgr[sn2]]
	       == 1 && grlibs[lgr[sn2]] == 1)){
		if(eyerec[sn2] != 0 && eyerec[sn2] != rn)
			deallocate_eye(eyerec[sn2]);
		addlist(sn2,&eyeptr[rn]);
		eyerec[sn2] = rn;
	      	numeyespots++;
	      	}
           else if(grcolor[board[sn2]] == 1-c)++numenemyspots;
	   else if(board[sn2] == NOGROUP)++numopenspots;
	   }


	if(numeyespots > 1){
		evalmanyeyespots(rn,sopen,c,numopenspots,numenemyspots,ldrno);
	   	}
	else{	/* ony one eyespot */
		/* put down stone to make eye and see if it is eye */
		mvs[msptr] = sopen;
		mvcolor[msptr] = c;
		flag = lupdate(msptr);
		upldrflags(msptr,ldrno);
		++msptr;
		count = getcount(s,c,rn,TRUE,ldrno);
		eyepot[rn] = onesp1[count];

		if(eyepot[rn] == 8){
			addlist(sopen,&eyevital[rn]);
			addlist(rn,&eyevitrec[sopen]);
			}
		if(eyepot[rn] != 0 && count != 7){
			i = fdir[s];
			for(ldtmp = ldiag[i]; i < ldtmp; ++i)
				if(board[s+diags[i]] == NOGROUP){
					addlist(s+diags[i],&eyevital[rn]);
					addlist(rn,&eyevitrec[s+diags[i]]);
					}
		    }

/* this code conflicts with the code in getarmytv_pot for extra eye potential
 *		if(eyepot[rn] == 8)
 *			for(ptr = nblbp[sopen]; ptr != EOL; ptr = links[ptr])
 *				if(list[ptr] != s && lnbn[list[ptr]] == 1 &&
 *				   lnbf[list[ptr]][1-c] == 0 && 
 *				   lnbf[list[nblbp[list[ptr]]]][1-c] == 0)
 *					eyepot[rn] += 4;
 */
		--msptr;
		ldndate(msptr);

		if(eyepot[rn] >= 4 && lnbf[sopen][1-c] == 0){
			mvcolor[msptr] = 1-c;   /* put down enemy stone */
			flag = lupdate(msptr);
			upldrflags(msptr,ldrno);

			++msptr;
			if(lnbn[sopen] == 2){
				for(ptr = nblbp[sopen]; ptr != EOL; ptr = links[ptr])
					if(list[ptr] != s)break;
				mvcolor[msptr] = c;
				mvs[msptr] = list[ptr];
				flag = lupdate(msptr);
				upldrflags(msptr,ldrno);
				++msptr;
				for(ptr = grnbp[board[sopen]]; ptr != EOL;
					ptr = links[ptr])
					if(grlibs[list[ptr]] == 1){
						flag = FALSE;
						break;
						}
				if(flag){
					tmpli = c7[getcount(sopen,c,rn,FALSE,ldrno)];
					if(tmpli > 1){
						if(eyepot[rn] == 8)
							eyeval[rn] = 8;
						else {
							eyepot[rn] = 8;
							}
						}
					}
				--msptr;
				ldndate(msptr);
				}
			else {
				if(iscaptured(board[sopen],9,playlevel,quicklibs,c,
					ldrno)){
					tmpli = c7[getcount(sopen,c,rn,FALSE,ldrno)];
					if(tmpli > 1){
						if(eyepot[rn] == 8)
							eyeval[rn] = 8;
						else {
							eyepot[rn] = 8;
							}
						}
					}
				else if(eyepot[rn] == 4)eyepot[rn] = 0;
				}
			--msptr;
			ldndate(msptr);
			}
		else if(eyepot[rn] == 4)eyepot[rn] = 0;


		if(eyeval[rn] == 8){  /* check if eye can be killed by putting group in atari */
			i = fdir[s];
			for(ldtmp = ldir[i]; i < ldtmp; ++i){
				sn = s + nbr[i];
				if(board[sn] != NOGROUP && grlibs[board[sn]] == 2){
					sn2 = list[grlbp[board[sn]]];
					if(sn2 == s)sn2 = list[links[grlbp[board[sn]]]];
					if(lnbn[sn2] > 1 || lnbf[sn2][1-c] != 0 && !onelibnbr(board[sn])){
						eyeval[rn] = 0;
						addlist(sn2,&eyevital[rn]);
						addlist(rn,&eyevitrec[sn2]);
						break;
						}
					}
				}
			}

		}
	}

int onelibnbr(int g){
	int ptr;
        for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
                if(grlibs[list[ptr]] == 1)return(TRUE);
        return(FALSE);
        }
