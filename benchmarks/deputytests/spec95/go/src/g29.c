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
# define NUMGEN 80
  
extern int cfac[];
extern int xtflag;
  
int numnn;
  
int mvl[NUMGEN];  /* move generated during lookahead */
int mvv[NUMGEN];  /* value of move generated */
int mvp;	  /* pointer into these arrays */

int maxnply[NUMPLY]; /* maximum nodes to try at this ply or below */
  
/* iscaptured does a tactical analysis of a group.  It returns true if
 * the group is captured and false if it can escape.  g is the group number
 * to be checked.  maxply, maxnodes, and maxlibs control the depth, size
 * and complexity of the search.  
 * maxply is the maximum depth of the search
 * maxnodes is the maximum number of nodes to look at not counting
 *   nodes where the move generator only generates one move
 *   (only counts nodes that branch)  MUST BE < 128!
 * maxlibs is the maximum number of liberties.  if the group gets more
 *   liberties than maxlibs it has escaped.
 * ctm is the color to move first.
 * ldrno is the index of this search in the grldr array
 * moves made in this search will be linked in to the
 * grldr and ldrflag lists unless ldrno is NOGROUP
 */

int iscaptured(int g,int maxply,int maxnodes,int maxlibs,int ctm,int ldrno)  {
	int goingup,done,ply,leaf,color,flag,starts;
	int nbptr,g2,el;
	el = grlibs[g];
	if(el <= maxlibs)
		el = getefflibs(g,maxlibs,ldrno);
	if(el > maxlibs){
		return(FALSE);
		}

   goingup = TRUE;
   numnn = 0;
   if(maxply > NUMPLY - 5)
      maxply = NUMPLY - 5;
   starts = mvs[grpieces[g]];
  
/*  mark neighboring groups */
  if(ldrno != NOGROUP)
	  for(nbptr = grnbp[g]; nbptr != EOL; nbptr = links[nbptr]){
		  g2 = list[nbptr];
		  if(grlibs[g2] > 4)continue;
  
		  if(addlist(ldrno,&ldrflag[mvs[grpieces[g2]]]))
			  adflist(mvs[grpieces[g2]],&grldr[ldrno]);
		  }
  
   done = FALSE;
   color = grcolor[g];
   ply = 1;
   sply[0] = msptr - 1;
   eply[0] = msptr;
   scrply[0] = -1;
   scrply[1] = -1;

   maxnply[1] = maxnodes;  

   while(!done){    /* this should be recursive */
      if(goingup){  /* just lupdated */
         if(grlibs[board[starts]] == 1 && color != ctm){
		 leaf = TRUE;
		 scrply[ply] = 1; /* dies */
		 }
	 else if(ply > maxply || numnn > maxnply[ply] || eply[ply-1] > MAXLADDER){
		 scrply[ply] = -1;  /* lives */
		 if(color == ctm)scrply[ply] = 1;
		 leaf = TRUE;
		 }
	 else
		 leaf = livesordies(starts,maxlibs,ctm,color,ply,ldrno);

         if(!leaf){
            maxnply[ply+1] = maxnply[ply];
            /* generate move list for this ply */
            if(ctm == color){
               	gendefmoves(ply,starts,g,ctm,ldrno);
		}
            else
               genatkmoves(ply,starts,g,ctm,ldrno);

            if(eply[ply] - sply[ply] == 1)--numnn;
  
            /* make best move */
            flag = FALSE;
            if(numnn <= maxnply[ply])
               while(!flag && sply[ply] < eply[ply]){
		  if(ldrno != NOGROUP)upldrflags(sply[ply],ldrno);
                  flag = lupdate(sply[ply]);
                  ctm = 1 - ctm;
                  if(mvs[sply[ply]] != PASS){
                     g2 = board[mvs[sply[ply]]];
		     if(g2 == board[starts] && grlibs[g2] == 1)
			     flag = FALSE;
                     if(grlibs[g2] == 1 && lnbn[list[grlbp[g2]]] > 1)
                        flag = FALSE;
                     }
                  if(!flag){
                     ldndate(sply[ply]);
                     ctm = 1 - ctm;
                     ++sply[ply];
                     }
                  }
            if(flag){
	       ++numnn;
               ++ply;
               scrply[ply] = -1;
               }
            else{
               scrply[ply] = -1;
               leaf = TRUE;
               }
            }
         if(leaf){
            if(ply == 1){
               done = TRUE;
               }
            else {
               --ply;
               ldndate(sply[ply]);
               ctm = 1 - ctm;
               ++sply[ply];
               goingup = FALSE;
               scrply[ply] = - scrply[ply+1];
               }
            }
         }
      else{        /* ldndated last */
         flag = FALSE;
	 maxnply[ply+1] = maxnply[ply];
         if(numnn <= maxnply[ply])
          while(!flag && sply[ply] != eply[ply] && scrply[ply] != 1){
            if(ldrno != NOGROUP)upldrflags(sply[ply],ldrno);
            flag = lupdate(sply[ply]);
            ctm = 1 - ctm;
            if(mvs[sply[ply]] != PASS){
               g2 = board[mvs[sply[ply]]];
	       if(g2 == board[starts] && grlibs[g2] == 1)
		       flag = FALSE;
               if(grlibs[g2] == 1 && lnbn[list[grlbp[g2]]] > 1)
                  flag = FALSE;
               }
            if(!flag){
               ldndate(sply[ply]);
               ctm = 1 - ctm;
               ++sply[ply];
               }
            }
         if(flag){
	    ++numnn;
            ++ply;
            scrply[ply] = -1;
            goingup = TRUE;
            }
         else if(ply == 1){
            done = TRUE;
	    }
         else {  /* ply is not 1 */
            --ply;
            ldndate(sply[ply]);
            ctm = 1 - ctm;
            ++sply[ply];
            scrply[ply] = - scrply[ply+1];
            }
         }
      }
	
	if(numnn > maxnodes){
		return(FALSE);
		}
	if(scrply[1] * cfac[ctm == color] == 1){
		return(FALSE);
		}
	else {
		return(TRUE);
		}
	}



/* generate moves to defend one liberty groups
 * rules:
 *	pull out of atari, if end with two liberties
 *	  by entering open space or connecting
 *	capture neighbor with one liberty
 */


void defonelib(int ply,int starts,int g,int ctm,int gs){
	int mvptr,ptr,i,s,connect,numlibs,twolibs,l1,l2;
	int ldtmp,c,sn,tmplist=EOL;
	mvptr = sply[ply];


	/* capture one liberty neighbor */

	for(ptr = grnbp[gs]; ptr != EOL; ptr = links[ptr])
		if(grlibs[list[ptr]] == 1 &&
			list[grlbp[list[ptr]]] != kosquare){
			mvs[mvptr] = list[grlbp[list[ptr]]];
			mvcolor[mvptr++] = ctm;
			}


	/* pull out of atari */

	c = grcolor[gs];
	s = list[grlbp[gs]];
	twolibs = lnbn[s] >= 2;
	connect = FALSE;
	if(lnbn[s] < 2 && lnbf[s][ctm] > 1){	/* can connect? */
		numlibs = lnbn[s];
		cpylist(nblbp[s],&tmplist);
		addlist(s,&tmplist);
		i = fdir[s];
		for(ldtmp = ldir[i]; i < ldtmp; ++i){
			sn = s + nbr[i];
			if(grcolor[board[sn]] == c && board[sn] != gs)
				numlibs += mrglist(grlbp[board[sn]],&tmplist);
			if(grcolor[board[sn]] == 1-c && grlibs[board[sn]] == 1)
				connect = TRUE;
			}
		if(numlibs > 1)connect = TRUE;
		killist(&tmplist);
		}
	if(lnbn[s] == 2 && lnbf[s][ctm] == 1 && !connect){
		l1 = list[nblbp[s]];
		l2 = list[links[nblbp[s]]];
		if(lnbf[l1][1-c] == 0 && lnbf[l2][1-c] == 0 && 
		   ((lnbn[l1] >= 2 && lnbn[l2] <= 2) ||
		    (lnbn[l2] >= 2 && lnbn[l1] <= 2)))twolibs = FALSE;
		}
	if(twolibs || connect){
		mvs[mvptr] = s;
		mvcolor[mvptr++] = ctm;
		}


	eply[ply] = mvptr;

	}



/* defend nbr of lastmove in atari 
 * defend if nbr has one liberty
 * and can get more by pulling out
 * or by capturing
 * makes double ataris work 
 */

int defatari(int ply,int g,int ctm,int gs,int lastmove){
	int i,j,ldtmp,s,ptr2,mvptr,sn,sn2,ldtm2,numlibs;
	if(lastmove == PASS)return(FALSE);
   	mvptr = sply[ply];
	i = fdir[lastmove];
	for(ldtmp = ldir[i]; i < ldtmp; ++i){
		s = lastmove + nbr[i];
		if(grcolor[board[s]] != grcolor[gs])continue;
		if(board[s] == gs)continue;
		if(grlibs[board[s]] > 1)continue;
		for(ptr2 = grnbp[board[s]]; ptr2 != EOL; ptr2 = links[ptr2])
			if(grlibs[list[ptr2]] == 1 && 
			   grsize[list[ptr2]] > 2)
				mvs[mvptr++] = list[grlbp[list[ptr2]]];
		numlibs = 0;
		sn = list[grlbp[board[s]]];
		numlibs += lnbn[sn];
		j = fdir[sn];
		for(ldtm2 = ldir[j]; j < ldtm2; ++j){
			sn2 = sn + nbr[j];
			if(grcolor[board[sn2]] == grcolor[gs] &&
			   grlibs[board[sn2]] >= 2){
				numlibs += grlibs[board[sn2]]-1;
			/* can connect */
				}
			if(grcolor[board[sn2]] == 1-grcolor[gs] &&
				grlibs[board[sn2]] == 1)
				numlibs += grsize[board[sn2]];
			}
		if(numlibs > 1)
			mvs[mvptr++] = sn;
		for(ptr2 = grnbp[board[s]]; ptr2 != EOL; ptr2 = links[ptr2])
			if(grlibs[list[ptr2]] == 1 && list[grlbp[list[ptr2]]]
			 != kosquare && list[grlbp[list[ptr2]]] != 
			  list[grlbp[board[s]]] && grsize[list[ptr2]] <= 2)
				mvs[mvptr++] = list[grlbp[list[ptr2]]];
		eply[ply] = mvptr;
		for(j = sply[ply]; j < eply[ply]; ++j){
			mvcolor[j] = ctm;
			}
		if(eply[ply] != sply[ply])return(TRUE);
		}
	return(FALSE);
	}


/* defender attack nbrs with fewer libs.
 * gs group being defended.  grl is the neighboring group to attack.
 * g is for debug output.
 * libamt is effective libs of gs
 * grllibs is effective libs of grl
 * only generate one attacking move for each nbr 
 * unless group chased has only two libs
 * don't generate attacking move which can be captured
 */

int def_atk_nbr(int gs,int grl,int mvp2,int g,int libamt,int grllibs,int ctm){
	int val,j,lptr2,illegal_move,ldtmp,mvvtmp,s,mvvmax,mvls,next_to;

	val = 150-10*grlibs[grl];  /* value of attack of grl */
	if(grlibs[grl] == 2)val += 50*grsize[grl];  /* attack is sente */

	if(grlibs[grl] == 1){ /* capture nbr */
            val = 100 * lnbn[list[grlbp[grl]]];
            if(grsize[grl] > 1){
               val = 1000+grsize[grl];
               }
            for(j = grnbp[grl]; j != EOL; j = links[j])
                if(list[j] != gs)	/* cutting stones */
                   val += 100 * grlibs[list[j]];
            }

	else if(grlibs[grl] < libamt)  /* have time to save nbr of nbr */
            for(lptr2 = grnbp[grl]; lptr2 != EOL;
               lptr2 = links[lptr2]){  /* save nbr of grl */
               if(list[lptr2] != gs && grlibs[list[lptr2]] == 1){
                  mvv[mvp2] = val + 100;
                  mvl[mvp2] = list[grlbp[list[lptr2]]];
                  mvp2++;
                  }
               }
	next_to = NOSQUARE;
	mvvmax = 0;
	for(lptr2 = grlbp[grl]; lptr2 != EOL;
            lptr2 = links[lptr2]){ /* take away a liberty from grl */

            mvvtmp = val + 30*lnbn[list[lptr2]] + edge[list[lptr2]];
	    if(lnbf[list[lptr2]][grcolor[grl]] > 1){
		j = fdir[list[lptr2]];
		for(ldtmp = ldir[j]; j < ldtmp; ++j){
			s = list[lptr2] + nbr[j];
			if(board[s] != grl && grcolor[board[s]] ==
				grcolor[grl])
				mvvtmp += 30*grlibs[board[s]];
				  /* make cutting move */
			}
		}


	    if(lnbn[list[lptr2]] == 1 && lnbf[list[lptr2]][ctm] == 0 &&
	       grlibs[grl] < libamt)
		next_to = list[nblbp[list[lptr2]]];


		/* don't gen illegal moves - move to spot with no
		liberties surrounded by enemies and chased group
		is not captured or trying to save group 
		also don't gen moves which are liberties of group
		chased since they have already been examined*/

	    if(lnbn[list[lptr2]] == 0 && grlibs[grl] > 1 && 
	       lnbf[list[lptr2]][ctm] == 0){
		    mvvtmp = 0;
		    continue;
		    }

            if(lnbn[list[lptr2]] <= 1 && grlibs[grl] > 1 && 
		grlibs[gs] <= grlibs[grl]){
               illegal_move = TRUE;
               j = fdir[list[lptr2]];
               for(ldtmp = ldir[j]; j < ldtmp; ++j){
                  s = list[lptr2]+nbr[j];
                  if(board[s] == grl)continue;
		  if(board[s] == gs && lnbn[list[lptr2]] == 0){
			illegal_move = TRUE;
			break;
			}
                  if(grcolor[board[s]] == grcolor[gs] &&
			(grlibs[board[s]] > 2 || lnbn[list[lptr2]] == 1)){
                     illegal_move = FALSE;
		     mvvtmp += 5 * grlibs[board[s]];
                     }
                  if(grcolor[board[s]] == 1-grcolor[gs] && 
			grlibs[board[s]] <= 2){
                        if(grlibs[board[s]] == 1 && kosquare != list[lptr2])
                           illegal_move = FALSE;
			mvvtmp += 50;
                        }
                  }
               if(illegal_move){
                  if(lnbn[list[lptr2]] == 0){
                     continue;
                     }
                  else mvvtmp = 0;
                  }
               }
	    if(grlibs[gs] == 2 && mvvtmp >= 200){
		mvv[mvp2] = mvvtmp;
		mvl[mvp2++] = list[lptr2];
		}
	    if(mvvtmp > mvvmax){
		mvvmax = mvvtmp;
                mvls = list[lptr2];
		}
            }
	if(mvvmax > 0 && (grlibs[gs] != 2 || mvvmax < 200)){
		mvv[mvp2] = mvvmax;
		mvl[mvp2++] = mvls;
		}
	else if(next_to != NOSQUARE){
		mvl[mvp2] = next_to;
		mvv[mvp2++] = val;
		}
	return(mvp2);
	}







   /* jump to escape */


int jump_to_escape(int gs,int mvp2){
	return(mvp2);	/* can't use ld[] here */
/*
	int i,s,lptr,ldtmp,sn,flag,j,ldtm2;   
      	for(lptr = grlbp[gs]; lptr != EOL;
          	lptr = links[lptr]){
         	s = list[lptr];
         	if(ld[s] <= 2 || edge[s] <= 1 || lnbn[s] <= 1)continue;
            	i = fdir[s];
            	for(ldtmp = ldir[i]; i < ldtmp; ++i){
               		sn = s + nbr[i];
               		if(board[sn] != NOGROUP)continue;
                  	flag = TRUE;
                  	j = fdir[sn];
                  	for(ldtm2 = ldir[j]; j < ldtm2; ++j)
                     		if(board[sn+nbr[j]] == gs)flag = FALSE;
                  	if(flag){
                     		mvl[mvp2] = sn;
                     		mvv[mvp2] = 300;
                     		++mvp2;
                     		}
                  	}
               	}
      	return(mvp2);
*/
      	}



   /* play next to group chased 
    * gs is group chased.  g is original group number for debug output.
    * defend group by extending in a liberty
    * get 100 points for each new liberty 
    */


int play_next_to_group(int gs,int mvp2,int g,int el){
   int passflag,poteyes,lptr,s,newlibs,flag,connlibs,enemy_nbr;
   int i,ldtmp,sn,grp,tmplist = EOL,ptr;
   int min_enemy_libs,makes_eye,connlist,c,ptr2;
   int eff_libs;  /* number of effective libs after move */
   passflag = TRUE;
   c = grcolor[gs];
   poteyes = 0;
   connlist = EOL;
   for(lptr = grlbp[gs]; lptr != EOL; lptr = links[lptr]){
      s = list[lptr];
      newlibs = newlist(grlbp[gs],nblbp[s]);
      eff_libs = el + newlibs - 1;
      if(newlibs > 1)passflag = FALSE;  /* don't pass if can get more libs*/
      mvv[mvp2] = newlibs * 100 - 100;
      mvl[mvp2] = s;
      if(edge[s] < 2)mvv[mvp2] -= 15;
      flag = FALSE;
      makes_eye = FALSE;
      connlibs = 0;	/* number of new libs for connection */
      enemy_nbr = 0;	/* most number of libs of enemy nbr */
      min_enemy_libs = 1000; /* least number of libs of enemy nbr */
      cpylist(grlbp[gs],&tmplist);
      i = fdir[s];
      for(ldtmp = ldir[i]; i < ldtmp; ++i){
         sn = s + nbr[i];
         grp = board[sn];
         if(grp != NOGROUP){
            if(grcolor[grp] != c){  /* found enemy group */
               if(grlibs[grp] > enemy_nbr)enemy_nbr = grlibs[grp];
	       if(grlibs[grp] < min_enemy_libs)
			min_enemy_libs = grlibs[grp];
               if(grlibs[grp] == 1)
                  mvv[mvp2] += grsize[grp] * 200;
               else if(grlibs[grp] <= grlibs[gs] + newlibs-1){
                  mvv[mvp2] += lnbn[s]*50;
                  }
               mvv[mvp2] -= 5 + grlibs[grp];
               }
            else if(grp != gs){	/* connect */
		connlibs += mrglist(grlbp[grp],&tmplist);
		if(lnbf[s][1-c] == 0 && lnbn[s] <= 1)addlist(grp,&connlist);
               	}
            }
         else{  /* sn is empty */
            if(lnbn[sn] == 1 && lnbf[sn][1-grcolor[gs]] == 0){
	       mvv[mvp2] += 100; /* makes a potential eye */
		makes_eye = TRUE;
		}
	    if(lnbn[sn] > 2 && !inlist(sn,&grlbp[gs])){
               mvv[mvp2] += lnbn[sn] * 50 - 100;
               flag = TRUE;
               }
            }
         }
      killist(&tmplist);
      eff_libs += connlibs;
      if(connlibs >= 2)passflag = FALSE;
      if(newlibs == 0 && enemy_nbr <= grlibs[gs])poteyes++;

      if(enemy_nbr && connlibs > 0 ||
         connlibs > 1)mvv[mvp2] += 100 * connlibs;
      if(connlibs == 1 && enemy_nbr <= grlibs[gs])mvv[mvp2] += 100;
			/* attack nbr and save group */
      if(connlibs > 2 && enemy_nbr)passflag = FALSE;
      if(min_enemy_libs < eff_libs)
	 mvv[mvp2] += 100;  /* attacking nbr group */
      if(flag)
         mvv[mvp2] -= 150;
      if(connlibs + newlibs >= 1 || lnbn[s] >= 2 || makes_eye)
	      ++mvp2;


      if(connlist != EOL){  /* play in liberty of group connected to */
	      for(ptr = connlist; ptr != EOL; ptr = links[ptr])
		      for(ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
			      if(list[ptr2] == s)continue;
			      mvv[mvp2] = 50;
			      mvl[mvp2++] = list[ptr2];
			      }
	      killist(&connlist);
	      }
      if(lnbn[s] == 1 && 
	 !enemy_nbr && grlibs[gs] > 1){	/* make eye */
	 if(board[list[nblbp[s]]] == NOGROUP){
		    mvl[mvp2] = list[nblbp[s]];
		    mvv[mvp2++] = 200;
		    }
         }
      if(edge[s] == 1 && lnbn[s] == 2 && grlibs[gs] > 2){ /* hane on edge */
	 for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
	    if(lnbn[list[ptr]] == 2 && lnbf[list[ptr]][1-grcolor[gs]] == 1){
		mvl[mvp2] = list[ptr];
		mvv[mvp2++] = 100;
		passflag = FALSE;
		}
	    }
	 }
      }
   if(passflag && grlibs[gs] < 4 && poteyes > 1){
      mvl[mvp2] = PASS;
      mvv[mvp2++] = 100;
      }
   return(mvp2);
   }


/* figure the number of effective liberties of group g
 * this is the number of moves it takes to capture g if
 * g's color passes every time or responds once.  Assume g's enemy plays
 * first.  Start with g's liberties. 
 * If g has two or more points where approach moves are
 * required, g's liberties are increased.  If g has two or more places where
 * he can connect for more liberties or extend for more liberties, g's 
 * liberties are increased.  If g has enemy neighbors in atari whose liberty
 * is not shared with g, g's liberties are increased.
 * can stop when get to n+1 libs
 */

int getefflibs(int g,int n,int ldrno){
	int el,ptr,s,sn,c,i,ldtmp,max,egrp,l1,l2;
	int friendly_capture,must_defend,defended,deflibs,undeflibs;
	int newlibs,bestdefended,numeyes,undefended[2],numdefended,tmplist;
	if(grlibs[g] == 1)return(1);
	if(grlibs[g] > n)return(grlibs[g]);
	if(grlibs[g] == 2){
		l1 = list[grlbp[g]];
		l2 = list[links[grlbp[g]]];
		if(lnbn[l1] != 0 && lnbn[l2] != 0)return(2);
		}
	c = grcolor[g];
	numeyes = 0;
	must_defend = 0;
	numdefended = 0;
	bestdefended = 0;
	undefended[0] = undefended[1] = 0;
	deflibs = 0;
	undeflibs = 0;
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		newlibs = 0;
		tmplist = EOL;
		defended = FALSE;
		friendly_capture = FALSE;

		if(lnbn[s] > 1 || lnbf[s][c] > 1){
			newlibs = lnbn[s];
			cpylist(nblbp[s],&tmplist);
			}

		if(lnbf[s][c] > 1){  /* check for connection, friendly capture */
			
			i = fdir[s];
			for(ldtmp = ldir[i]; i < ldtmp; ++i){
				sn = s + nbr[i];
				if(grcolor[board[sn]] == c && board[sn] != g){
					if(grlibs[board[sn]] == 1 && kosquare != s){
						friendly_capture = TRUE;
						}
					newlibs += mrglist(grlbp[board[sn]],
							     &tmplist);

					if(ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[board[sn]]]]))
						adflist(mvs[grpieces[board[sn]]],&grldr[ldrno]);
					}
				}
			}

		if(lnbn[s] <= 1 && lnbf[s][1-c] == 0 && !friendly_capture)
			defended = TRUE;

		if(lnbn[s] == 0 && lnbf[s][1-c] == 1 && !friendly_capture){ 
			                     /* defended due to shortage of libs */
			egrp = NOGROUP;
			i = fdir[s];
			for(ldtmp = ldir[i]; i < ldtmp; ++i){
				sn = s + nbr[i];
				if(grcolor[board[sn]] == 1-c && grlibs[board[sn]] <= 2){
					if(egrp != NOGROUP && egrp != board[sn]){
						defended = FALSE;
						break;
						}
					defended = TRUE;
					egrp = board[sn];
					if(ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[board[sn]]]]))
						adflist(mvs[grpieces[board[sn]]],&grldr[ldrno]);
					}
				}
			}

		if(newlibs > 0)newlibs -= grlibs[g] - mrglist(grlbp[g],&tmplist);
		if(lnbn[s] == 0 && lnbf[s][1-c] == 0 && !friendly_capture)
			numeyes++;
		--newlibs;  /* since have to fill a lib to get new libs */
		if(tmplist != EOL)killist(&tmplist);
		if(defended && newlibs > bestdefended){
			bestdefended = newlibs;
			deflibs = grlibs[g]+bestdefended-1;
			if(deflibs > n)return(deflibs);
			}
		if(!defended && newlibs > 0){
			if(newlibs > undefended[0]){
				undefended[1] = undefended[0];
				undefended[0] = newlibs;
				}
			else if(newlibs > undefended[1])
				undefended[1] = newlibs;
			undeflibs = grlibs[g]+undefended[1]-1;
			if(undeflibs > n)return(undeflibs);
			}
		if(defended)++numdefended;
		}

	must_defend = FALSE;
	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
		if(grlibs[list[ptr]] == 1){  /* can capture */
			if(ldrno != NOGROUP && addlist(ldrno,&ldrflag[mvs[grpieces[list[ptr]]]]))
				adflist(mvs[grpieces[list[ptr]]],&grldr[ldrno]);
			newlibs = grsize[list[ptr]];
			if(inlist(list[grlbp[list[ptr]]],&grlbp[g]))--newlibs;
			else must_defend = TRUE;
			if(newlibs > undefended[0]){
				undefended[1] = undefended[0];
				undefended[0] = newlibs;
				}
			else if(newlibs > undefended[1])
				undefended[1] = newlibs;
			undeflibs = grlibs[g]+undefended[1]-1;
			if(undeflibs > n)return(undeflibs);
			if(newlibs > 1)must_defend = TRUE;
			}

	el = grlibs[g] + must_defend;  /* option 1, let him fill all liberties */
	if(numeyes <= 1 && numdefended > 1)el += numdefended-1;
	if(numeyes >= 2)el += numdefended;
	max = el;


	return(max);
	}

/* generate moves to attack a 2 liberty group (g) to defend group (gs)
 * return TRUE if moves were generated
 */


int attack2libs(int gs,int g,int* mvp2,int ctm){
	int ptr,connflag,nearflag;
	int val,i,ldtmp,s,sn,retval = FALSE;


	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		val = 0;
		nearflag = FALSE;
		connflag = FALSE;
		s = list[ptr];
		if(lnbn[s] <= 1 && lnbf[s][ctm] == 0)continue;
		if(inlist(s,&grlbp[gs]))nearflag = TRUE;
		i = fdir[s];
		for(ldtmp = ldir[i]; i < ldtmp; ++i){
			sn = s + nbr[i];
			if(board[sn] == NOGROUP && inlist(sn,&grlbp[gs]))
				nearflag = TRUE;
			if(grcolor[board[sn]] == ctm && board[sn] != gs)
				connflag = TRUE;
			if(grcolor[board[sn]] == 1-ctm && board[sn] != g){
				if(grlibs[board[sn]] == 2)val = 400;  /* double atari */
				else if(grlibs[board[sn]] == 3)val = 200;
				}
			}
		if(nearflag){
			val += 100*lnbn[s];
			if(connflag)val += 200;
			}
		if(val != 0){
			mvv[*mvp2] = val;
			mvl[(*mvp2)++] = s;
			retval = TRUE;
			}
		}
	return(retval);
	}


/* generate moves to defend group at starts 
 * if last move was atari, defend group in atari
 * if group has one liberty, call defonelib
 * if group has 2 liberties and no weak neighbors, call deftwolib
 * otherwise:
 *   try to jump to escape
 *   try to extend for more liberties
 *   try to attack a weak neighbor
 */

void gendefmoves(int ply,int starts,int g,int ctm,int ldrno){
   int mvp2,mvptr,ptr,common;
   int j,k,best,num,mvall,bestval,gs;
   int can_attack_nbr;
   int lastmove,eff_libs,nbeff_libs,two_libs_attack;

   lastmove = mvs[sply[ply-1]];

   gs = board[starts];
   sply[ply] = eply[ply-1];
   mvptr = sply[ply];

   if(ply > 1 && defatari(ply,g,ctm,gs,lastmove))return;

   if(grlibs[gs] == 1){
	defonelib(ply,starts,g,ctm,gs);
	return;
	}

   mvp2 = 0;
   eff_libs = getefflibs(gs,taclibs,ldrno);
   can_attack_nbr = FALSE;
   two_libs_attack = FALSE;
   for(ptr = grnbp[gs]; ptr != EOL; ptr = links[ptr]){
	if(grlibs[list[ptr]] <= eff_libs && 
	   (nbeff_libs = getefflibs(list[ptr],eff_libs,ldrno)) <= eff_libs){
		can_attack_nbr = TRUE;
		mvp2 = def_atk_nbr(gs,list[ptr],mvp2,g,eff_libs,nbeff_libs,ctm);
		continue;
		}
	if(grlibs[list[ptr]] == 2 && attack2libs(gs,list[ptr],&mvp2,ctm))
	   two_libs_attack = TRUE;
	}


   if(!can_attack_nbr && !two_libs_attack && grlibs[gs] == 2){
	deftwolibs(ply,starts,g,ctm,gs);
	return;
	}


   if(ply <= 2 && grlibs[gs] > 2)
	   mvp2 = jump_to_escape(gs,mvp2);

   mvp2 = play_next_to_group(gs,mvp2,g,eff_libs);

   if(mvp2 == 0){  /* haven't found any moves yet, try for seki */
	   for(ptr = grnbp[gs]; ptr != EOL; ptr = links[ptr]){
		   common = comlist(grlbp[gs],grlbp[list[ptr]]);
		   if(common >= 1 && grlibs[list[ptr]] <= eff_libs + common)
			   mvp2 = def_atk_nbr(gs,list[ptr],mvp2,g,eff_libs,grlibs[list[ptr]],ctm);
		   }
           }


   for(k = 0; k < mvp2; ++k){	/* combine duplicates */
      for(j = k+1; j < mvp2; ++j)
         if(mvl[j] == mvl[k]){
            mvv[k] += mvv[j];
            mvv[j] = -1000;
            }

      if(mvl[k] == kosquare){
         mvv[k] = -1000;
         continue;
         }
      }

   mvall = mvp2;
   if(ply > 3 && mvall > mvmost)
      mvall = mvmost;        /* only generate mvmost best moves*/
   if(mvall > mvmost+2)mvall = mvmost+2;
   for(k = 0; k < mvall; ++k){
      best = -1000;
      num = 0;
      for(j = 0; j < mvp2; ++j)
         if(mvv[j] > best){
            best = mvv[j];
            num = j;
            }
      if(k == 0)bestval = mvv[num];
      if(mvv[num] < -playlevel && k != 0 || (mvv[num] < bestval-900-playlevel && 
          mvl[num] != PASS)){
         break;
         }
      mvv[num] = -2000;
      mvs[mvptr] = mvl[num];
      mvcolor[mvptr] = ctm;
      ++mvptr;
      }
   eply[ply] = mvptr;
   }





/* generate moves from two stone wall.  rules:
 * at edge of board, just try hane if it connects.
 * if both sides open, try both jumps.
 * if one side open, try jumping on other.
 * if both sides open, hane if cannects, or attacks weak nbr
 */

int bugfixIndex(int index);    // sm:

int def_two_stone_wall(int l1,int l2,int gs,int mvptr){
	int hane;

	if(lnbn[l1] < 3 && lnbn[l2] < 3)
		return(mvptr);  /*hosed*/
        if(lnbn[l1] == 2 && board[ bugfixIndex(l1+l1-l2) ] != NOGROUP && lnbn[l2] == 3)
		mvs[mvptr++] = jump(l1,gs);
	else if(lnbn[l1] == 3 && lnbn[l2] == 2 && board[l2+l2-l1] != NOGROUP)
		mvs[mvptr++] = jump(l2,gs);
	if(lnbn[l1] != 3 || lnbn[l2] != 3)
		return(mvptr);
	hane = l1+l1-l2;
	if(lnbf[hane][grcolor[gs]] != 0 ||
		lnbn[hane] == 3)
		mvs[mvptr++] = hane;	
	hane = l2+l2-l1;
	if(lnbf[hane][grcolor[gs]] != 0 ||
		lnbn[hane] == 3)
		mvs[mvptr++] = hane;	
	mvs[mvptr++] = jump(l1,gs);
	mvs[mvptr++] = jump(l2,gs);
	return(mvptr);
	}

/* find jumping move from liberty relative to g */

int jump(int lib,int g){
	int i,sn,ldtmp;
	i = fdir[lib];
	for(ldtmp = ldir[i]; i < ldtmp; ++i){
		sn = lib + nbr[i];
		if(board[sn] == g)return(lib - nbr[i]);
		}
	return(lib);
	}


/* generate defensive moves for two liberty group which cannot
 * attack a neighbor
 * rules:
 *	jump from two stone wall
 *	if both liberties have 3 libs of libs and liberties are adjacent
 *		try hane.
 *      potential eye is liberty surrounded by friendly stones
 *	if two potential eyes, pass
 *	if can't get more liberties, pass
 *	extend or connect for most liberties
 *      if a lib can be surrounded to make a potential eye, do it
 *	if liberty has two adjacent friendly stones and no enemies, hane to
 *		try to make an eye.
 */

void deftwolibs(int ply,int starts,int g,int ctm,int gs){
	int mvptr,libs[2],newlibs[2],i,j,ldtmp,s,ptr,ptr2;
	int pot_eye[2],twostonewall,can_connect,connlibs,cgroup,can_atari;

      	mvptr = sply[ply];
      	libs[0] = list[grlbp[gs]];		/* find liberties */
      	libs[1] = list[links[grlbp[gs]]];

	can_connect = FALSE;
	twostonewall = abs(libs[0]-libs[1]) == 1 || 
		abs(libs[0]-libs[1]) == boardsize;

	pot_eye[0] = lnbf[libs[0]][1-ctm] == 0 && lnbn[libs[0]] == 0;
	pot_eye[1] = lnbf[libs[1]][1-ctm] == 0 && lnbn[libs[1]] == 0;
	can_atari = FALSE;

	for(i = 0; i < 2; ++i){
		newlibs[i] = 0;
		cgroup = NOGROUP;
		j = fdir[libs[i]];
		for(ldtmp = ldir[j]; j < ldtmp; ++j){
			s = libs[i]+nbr[j];
			if(grcolor[board[s]] == ctm && board[s] != gs){
				connlibs = grlibs[board[s]] - 1;
				if(connlibs && board[s] != cgroup){
					can_connect = TRUE;
					cgroup = board[s];
					newlibs[i] += connlibs+1; /* bonus for conn */
					if(lnbn[libs[i]] <= 1 && 
					   lnbf[libs[i]][1-grcolor[gs]] == 0)
						for(ptr = grlbp[cgroup]; ptr != EOL; ptr = links[ptr])
							if(list[ptr] != libs[i])
								mvs[mvptr++] = list[ptr];  /* play from unbreakable connection */
					}
				}
			else if(board[s] == NOGROUP && s != libs[1-i]){
				newlibs[i]++;
				if(lnbf[s][ctm] != 0 && lnbn[s] <= 2 &&
				   lnbf[s][1-ctm] == 0){
					can_connect = TRUE;
					mvs[mvptr++] = libs[i];
					}
				if(lnbf[s][1-ctm] == 0 && lnbn[s] == 1)
					newlibs[i]++;
				}
			else if(grcolor[board[s]] == 1-ctm && grlibs[board[s]] <= 2){
				if(grlibs[board[s]] == 1 && !inlist(board[s],&grnbp[gs]))
					newlibs[i]++;
				can_atari = TRUE;
				}

			}
		newlibs[i]--;
		}



	if(twostonewall && !can_connect)
		mvptr = def_two_stone_wall(libs[0],libs[1],gs,mvptr);

	if(pot_eye[0] && pot_eye[1])
		mvs[mvptr++] = PASS;


	else if(newlibs[0] == newlibs[1] && newlibs[0] > 0 || can_atari){
		if(!pot_eye[0]){
			mvs[mvptr++] = libs[0];
			mvs[mvptr++] = libs[1];
			}
		else if(!pot_eye[1]){
			mvs[mvptr++] = libs[1];
			mvs[mvptr++] = libs[0];
			}
		}

	else if(newlibs[0] > 0 && newlibs[0] > newlibs[1]){
		mvs[mvptr++] = libs[0];
		if(pot_eye[0])
			mvs[mvptr++] = libs[1];
		}
	else if(newlibs[1] > 0){
		mvs[mvptr++] = libs[1];
		if(pot_eye[1])
			mvs[mvptr++] = libs[0];
		}

	else if(lnbn[libs[0]] == 0 && lnbn[libs[1]] == 0 ||
		twostonewall && lnbn[libs[0]] < 2 && lnbn[libs[1]] < 2){
		mvs[mvptr++] = PASS;
		}
	for(i = 0; i < 2; ++i){ /* make eye */
		if(lnbf[libs[i]][1-tm] != 0 || lnbn[libs[i]] != 1)continue; 
		mvs[mvptr++] = list[nblbp[libs[i]]];
		for(ptr = nblbp[list[nblbp[libs[i]]]]; ptr != EOL; ptr = links[ptr])
			if(lnbf[list[ptr]][1-grcolor[gs]] != 0)
				mvs[mvptr++] = list[ptr];
		}

	for(i = 0; i < 2; ++i)
		if(lnbn[libs[i]] == 2 && lnbf[libs[i]][1-ctm] == 0 && !can_connect)
			for(ptr = nblbp[libs[i]]; ptr != EOL; ptr = links[ptr]){
				if(lnbf[list[ptr]][ctm] != 0){
					mvs[mvptr++] = list[ptr];
					continue;
					}
				if(lnbn[list[ptr]] == 4){
					mvs[mvptr++] = list[ptr];
					continue;
					}
				if(lnbn[list[ptr]] <= 1)continue;
				for(ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
					if(list[ptr2] == libs[i])continue;
					if(lnbn[list[ptr2]] <= 2 && lnbf[list[ptr2]][1-ctm] == 0){
						mvs[mvptr++] = list[ptr];
						break;
						}
					}
				}
	

	eply[ply] = mvptr;
	for(i = sply[ply]; i < eply[ply]; ++i){
		mvcolor[i] = ctm;
		}
	}




/* generate attack moves for two liberty groups 
 * generate moves for simple attacks of two liberty groups
 * if can't find simple attack, return false and let genatkmoves
 * generate the moves.
 * rules:
 *	try to geta the group if liberties are on diagonal and
 *		have two liberty liberties each and all nbr groups
 *		have more than two liberties and there are no
 *		connections to friendly groups
 *	try for loose ladder if libs on diagonal, all nbr groups
 *  		have more than 2 libs and no conns to friendly groups
 *	fill liberty which cuts connection to group with more liberties
 *	if there are more than 2 neighboring groups with 2 liberties
 *		then must fill a liberty
 *	if liberties are adjacent then filling lib will reduce nbn of
 *		other liberty
 *	fill liberty with highest nbn first
 *	if there are more than two nbr groups with 2 liberties
 *		then must not leave a liberty with nbn = 3
 *	Don't fill a liberty which will lead to immediate capture unless
 *		throw in works
 *	Throw in works if other liberty is adjacent to this liberty and
 *		move is a cut or group being cut from has few liberties
 *              and other libs adjacent to thislib are not new.
 *	Throw in also works if move is cut and other liberty is not
		throw in
 *	If both liberties are protected and no throw in works, play next
 *		to a liberty
 */

int atktwolibs(int ply,int starts,int g,int ctm,int gs){
	int libs[2],cut[2],getaflag,i,ldtmp,libs_adjacent,j,sn,ptr,gn;
	int push[2],capture[2],throwin[2],illegal[2];
	int connect[2],pushfrom[2],val[2],c,num2libs = 0,libs_diagonal;
	int hole[2];  /* pushing this way goes into hole */

	c = grcolor[gs];
	for(ptr = grnbp[gs]; ptr != EOL; ptr = links[ptr]){
		gn = list[ptr];
		if(grlibs[gn] == 1)return(FALSE); /* too complicated */
		if(grlibs[gn] == 2)++num2libs;
		}

      	libs[0] = list[grlbp[gs]];		/* find liberties */
      	libs[1] = list[links[grlbp[gs]]];
      	cut[0] = cut[1] = FALSE;
	push[0] = push[1] = FALSE;
	connect[0] = connect[1] = FALSE;
	capture[0] = capture[1] = FALSE;
	throwin[0] = throwin[1] = FALSE;  /* indicates spot has one liberty */
	illegal[0] = illegal[1] = FALSE;
	pushfrom[0] = pushfrom[1] = NOGROUP;

	libs_diagonal = libs[1] - libs[0] == boardsize-1 || 
		libs[1] - libs[0] == boardsize+1;
	libs_adjacent = libs[1] - libs[0] == 1 || libs[1] - libs[0] == boardsize;


	if(libs_diagonal && num2libs == 0 && lnbn[libs[0]] == 2 && lnbn[libs[1]] == 2 &&
	   lnbf[libs[0]][c] == 1 && lnbf[libs[1]][c] == 1){
		sn = NOSQUARE;
		if(libs[1]-libs[0] == boardsize-1){
			if(board[libs[0]-1] == NOGROUP)
				sn = libs[0]-1;
			if(board[libs[1]+1] == NOGROUP)
				sn = libs[1]+1;
			}
		if(libs[1]-libs[0] == boardsize+1){
			if(board[libs[0]+1] == NOGROUP)
				sn = libs[0]+1;
			if(board[libs[1]-1] == NOGROUP)
				sn = libs[1]-1;
			}
		if(sn != NOSQUARE && lnbf[sn][1-c] < 2){
			mvl[mvp] = sn;
			mvv[mvp++] = 500;
			return(TRUE);
			}
		}

	if(libs_diagonal && (lnbf[libs[0]][c] == 1 || lnbf[libs[1]][c] == 1)){
		/* check for ladder */
		for(i = 0; i < 2; ++i)
			if(lnbn[libs[i]] == 3 && lnbn[libs[1-i]] == 2){
				mvl[mvp] = libs[i];
				mvv[mvp++] = 500;
				return(TRUE);
				}
		}
			

	/* find out if filling this liberty is a cut */
	/* find out if stone here pushes from friendly stone */

	for(i = 0; i < 2; ++i){
		hole[i] = FALSE;
		j = fdir[libs[i]];
		for(ldtmp = ldir[j]; j < ldtmp; ++j){
			sn = libs[i] + nbr[j];
			if(board[sn] == gs)continue;
			if(grcolor[board[sn]] == grcolor[gs]){
				cut[i] = TRUE;
				if(grlibs[board[sn]] == 1)
					capture[i] = TRUE;
				}
			else if(grcolor[board[sn]] == 1-grcolor[gs]){
				push[i] = TRUE;
				if(board[sn] != pushfrom[i] &&
				    pushfrom[i] != NOGROUP)connect[i] = TRUE;
				pushfrom[i] = board[sn];
				}
			else if(lnbn[sn] <= 2 && lnbf[sn][grcolor[gs]] == 0)
				hole[i] = TRUE;
			}
		}


	/* find out if move is illegal or can be immediately captured */

	for(i = 0; i < 2; ++i){
		if(!push[i] && !capture[i] && lnbn[libs[i]] == 1) 
			throwin[i] = TRUE;
		if(!push[i] && !capture[i] && lnbn[libs[i]] == 0 ||
			kosquare == libs[i])
			illegal[i] = TRUE;
		if(push[i] && !capture[i] && !connect[i] &&
			grlibs[pushfrom[i]] + lnbn[libs[i]] <= 2) 
			illegal[i] = TRUE;
		}


	/* defend nbrs with one liberty 
	currently returning since this is too complicated!
	
	for(ptr = grnbp[gs]; ptr != EOL; ptr = links[ptr]){
		gn = list[ptr];
		if(grlibs[gn] != 1)continue;

		for(ptr2 = grnbp[gn]; ptr2 != EOL; ptr2 = links[ptr2]){
			sn = list[grlbp[list[ptr2]]];
			if(grlibs[list[ptr2]] == 1 && sn != kosquare){
				mvl[mvp] = sn;
				mvv[mvp++] = 100;
				if(sn == libs[0])illegal[0] = TRUE;
				if(sn == libs[1])illegal[1] = TRUE;
				}
			}
		sn = list[grlbp[gn]];
		if(sn == libs[0])illegal[0] = TRUE;
		if(sn == libs[1])illegal[1] = TRUE;
		mvl[mvp] = list[grlbp[gn]];
		mvv[mvp++] = 100;
		}
	*/

	/* pick order for filling in the two liberties */

	/* make cutting move first (includes throw ins) */

	if(cut[0] && !illegal[0] && (!throwin[0] ||
		libs_adjacent || lnbn[list[nblbp[libs[0]]]] <= 2)){
		mvl[mvp] = libs[0];
		mvv[mvp++] = 500;
		illegal[0] = TRUE;
		}
	if(cut[1] && !illegal[1] && (!throwin[1] ||
		libs_adjacent || lnbn[list[nblbp[libs[1]]]] <= 2)){
		mvl[mvp] = libs[1];
		mvv[mvp++] = 500;
		illegal[1] = TRUE;
		}


	/* make move which takes away most liberties of liberties */
	/* if equal, best move pushes from friendly stone */
	/* if equal, move on side away from edge */

	for(i = 0; i < 2; ++i)
		val[i] = 0;
	for(i = 0; i < 2; ++i){
		val[i] = 0;
		if(illegal[i])continue;
		val[i] += lnbn[libs[i]]*100;
		if(push[i] && grlibs[pushfrom[i]] < 3)val[i] += 100;
		if(push[i])val[i] += 50;
		if(hole[i])val[1-i] += 25;
		}
	if(edge[libs[0]] > edge[libs[1]])
		val[0] += 10;
	else if(edge[libs[1]] > edge[libs[0]])
		val[1] += 10;


	if(!illegal[1] && !throwin[1]){
		mvl[mvp] = libs[1];
		mvv[mvp++] = val[1];
		illegal[1] = TRUE;
		}
	if(!illegal[0] && !throwin[0]){
		mvl[mvp] = libs[0];
		mvv[mvp++] = val[0];
		illegal[0] = TRUE;
		}


	/* move next to throwin spot */

	if(throwin[0] && illegal[0]){
		mvl[mvp] = list[nblbp[libs[0]]];
		mvv[mvp++] = 250;
		}
	if(throwin[1] && illegal[1]){
		mvl[mvp] = list[nblbp[libs[1]]];
		mvv[mvp++] = 250;
		}



   		/* try for geta or loose ladder */

      	if(!cut[0] && !cut[1] && !libs_adjacent &&
	 	(lnbn[libs[0]] == 2 || lnbn[libs[1]] == 2)){
	      	getaflag = TRUE;
         	for(i = 0; i < 2; ++i)
            		if(push[i] && grlibs[pushfrom[i]] < 3){
               			getaflag = FALSE;
               			break;	
			/* no geta if adjacent group only 2 or 1 libs */
               		}
         	if(getaflag){
            		if(libs[1]-libs[0] == boardsize-1){
               			if(board[libs[0]-1] == NOGROUP){
                  			mvl[mvp] = libs[0]-1;
					mvv[mvp++] = 250;
                  			}
               			if(board[libs[1]+1] == NOGROUP){
                  			mvl[mvp] = libs[1]+1;
					mvv[mvp++] = 250;
                  			}
               			}
            		if(libs[1]-libs[0] == boardsize+1){
               			if(board[libs[0]+1] == NOGROUP){
                  			mvl[mvp] = libs[0]+1;
					mvv[mvp++] = 250;
                  			}
               			if(board[libs[1]-1] == NOGROUP){
                  			mvl[mvp] = libs[1]-1;
					mvv[mvp++] = 250;
                  			}
               			}
            		}
         	}
	return(TRUE);
	}

/* generate moves to attack group at starts */

void genatkmoves(int ply,int starts,int g,int ctm,int ldrno){
   int mvptr;
   int j,k,best,num,mvall,bestval,gs;
   int diff;

   gs = board[starts];
   sply[ply] = eply[ply-1];
   mvp = 0;

   
	if(grlibs[gs] == 2){
		if(!atktwolibs(ply,starts,g,ctm,gs))
			genrestatk(ply,starts,g,ctm,gs,ldrno);
		}
   	else {
		genrestatk(ply,starts,g,ctm,gs,ldrno);
	}
   mvptr = sply[ply];

   /* make move list */


   for(k = 0; k < mvp; ++k){	/* combine duplicates */
      for(j = k+1; j < mvp; ++j)
         if(mvl[j] == mvl[k]){
            mvv[k] += mvv[j];
            mvv[j] = -1000;
            }

      if(mvl[k] == kosquare){
         mvv[k] = -1000;
         continue;
         }
      }

   mvall = mvp;
   if(ply > 1 && mvall > mvmost)
      mvall = mvmost;        /* only generate mvmost best moves*/
   if(mvall > mvmost+2)mvall = mvmost+2;
   for(k = 0; k < mvall; ++k){
      best = -1000;
      num = 0;
      for(j = 0; j < mvp; ++j)
         if(mvv[j] > best){
            best = mvv[j];
            num = j;
            }
      if(k == 0)bestval = mvv[num];
      if(k == 1)diff = bestval - mvv[num];
      if(mvv[num] < -playlevel && k != 0 || mvv[num] < bestval-400-playlevel){
         break;
         }
      mvv[num] = -2000;
      mvs[mvptr] = mvl[num];
      mvcolor[mvptr] = ctm;
      ++mvptr;
      }
   eply[ply] = mvptr;
   if(eply[ply] - sply[ply] > 1){
	if(maxnply[ply] - numnn > 20 && (diff < 50 || maxnply[ply] > 50)){
		if(diff < 50 && maxnply[ply] > 50)
			maxnply[ply+1] = maxnply[ply] - 20;
		maxnply[ply+1] = maxnply[ply] - 10;
		}
	}
   }


/* generate general attacking moves */


void genrestatk(int ply,int starts,int g,int ctm,int gs,int ldrno){
   int lptr,lptr2,flag,s,i,ldtmp,grp,libamt;
   int val,j,sn,grl,throwin,sumlibs;
   int numlibs,nbflag,tmplist = EOL,c,mvval,sn2,captureflag,libval;
   int s1,s2,canatari,cancapture;

   c = grcolor[gs];

   /* attack from a distance (commented out)
   
   if(ply <= 2)
      for(lptr = grlbp[gs]; lptr != EOL;
          lptr = links[lptr]){
         s = list[lptr];
         if(edge[s] > 1 && lnbn[s] > 2 (need more stuff here)){
            i = fdir[s];
            for(ldtmp = ldir[i]; i < ldtmp; ++i){
               sn = s + nbr[i];
               if(board[sn] == NOGROUP){
                  flag = TRUE;
                  j = fdir[sn];
                  for(ldtm2 = ldir[j]; j < ldtm2; ++j)
                     if(board[sn+nbr[j]] == gs)flag = FALSE;
                  if(flag){
                     mvl[mvp] = sn;
                     mvv[mvp] = 200;
                     ++mvp;
                     }
                  }
               }
            }
         }

   */

   /* play next to group chased, take away a liberty */
   for(lptr = grlbp[gs]; lptr != EOL; lptr = links[lptr]){
      s = list[lptr];
      mvval = newlist(grlbp[gs],nblbp[s]) * 100 - 100;
	/* number of new liberties would get from playing here */
      throwin = FALSE;

      if(lnbn[s] == 0){
         captureflag = canatari = FALSE;
         sumlibs = 0;
         i = fdir[s];
         for(ldtmp = ldir[i]; i < ldtmp; ++i){
            sn = s + nbr[i];
            if(grcolor[board[sn]] == ctm)sumlibs += grlibs[board[sn]]-1;
            if(grcolor[board[sn]] == 1-ctm){
               if(grlibs[board[sn]] == 1)captureflag = TRUE;
               if(grlibs[board[sn]] == 2)canatari = TRUE;
               }
            }

	 /* see if need to connect before filling liberty */
	 if(sumlibs == 1 && captureflag == FALSE){
  		i = fdir[s];
         	for(ldtmp = ldir[i]; i < ldtmp; ++i){
            		sn = s + nbr[i];
            		if(grcolor[board[sn]] == ctm && grlibs[board[sn]] == 2){
				sn2 = list[grlbp[board[sn]]];
				if(sn2 != s){
					mvv[mvp] = 50;
					mvl[mvp++] = sn2;
					}
				sn2 = list[links[grlbp[board[sn]]]];
				if(sn2 != s){
					mvv[mvp] = 50;
					mvl[mvp++] = sn2;
					}
				}
	    		}
		}

         if(!captureflag && sumlibs + canatari >= grlibs[gs]){
		throwin = TRUE;
		}
         if(!captureflag && (sumlibs <= 1 || sumlibs < grlibs[gs] &&
		!canatari)){
            continue;
            }
         }

      else if(lnbn[s] == 1){
         throwin = TRUE;
         i = fdir[s];
         for(ldtmp = ldir[i]; i < ldtmp; ++i){
            sn = s + nbr[i];
            if(board[sn] == NOGROUP){
               mvv[mvp] = 100;
               mvl[mvp] = sn;
               }
            else if(grcolor[board[sn]] == ctm &&
               grlibs[board[sn]] >= 2){
               throwin = FALSE;
               break;
               }
            }
         if(throwin){
            ++mvp; /* move next to one lib spot */
            if(grlibs[gs] > 2)mvval -= 100;
            }
         }
      else if(lnbn[s] < grlibs[gs])
         mvval += 50;
      else   /* lnbn > 1 */
         mvval += 100;
      mvl[mvp] = s;
      mvv[mvp] = mvval;
      if(edge[s] == 0)mvv[mvp] -= 30;
      if(edge[s] == 1){
		mvv[mvp] -= 15;
		if(lnbn[s] == 2){
			s1 = list[nblbp[s]];
			s2 = list[links[nblbp[s]]];
			if(lnbf[s1][1-c] != 0 && lnbf[s2][1-c] != 0)
				mvv[mvp] += 50;
			}
		}

      flag = 0;
      i = fdir[s];
      for(ldtmp = ldir[i]; i < ldtmp; ++i){
         sn = s + nbr[i];
         grp = board[sn];
         if(grp != NOGROUP){
            if(grcolor[grp] == ctm){
               if(grlibs[grp] < grlibs[gs]){
                  mvv[mvp] += lnbn[s]*50 - 25;
                  if(grlibs[grp] == 1 && grsize[grp] > 1)
                     mvv[mvp] += 600;
                  }
	       else mvv[mvp] += 50;
               mvv[mvp] -= 5 + grlibs[grp];
               }
            else if(grp != gs){	/* cut */
               mvv[mvp] += 100 * grlibs[grp] - 100;
	       if(grlibs[grp] == 2)mvv[mvp] += 200;  /* cut is atari */
	       }
            }

/* this code is totally wrong since ld and lgr are invalid during lookahead!
         else if(!throwin &&
              (ld[sn] == NEUTRALLD || 
		grcolor[lgr[sn]] == 1-ctm))  
              mvv[mvp] += 150;
 */

         else if(lnbn[sn] == 4){
              mvv[mvp] += 50;
              flag++;
              }
         }
      if(flag > 1)
         mvv[mvp] += 100;  /* can't stop from making one more liberty if
                              plays here */
      ++mvp;
      }



   /*  defend nbrs with fewer libs */
   libamt = grlibs[gs]-1;
   if(libamt > 0){
      for(lptr = grnbp[gs]; lptr != EOL; lptr = links[lptr]){
         if(grlibs[list[lptr]] <= libamt && (grlibs[list[lptr]] == 1 ||
	    getefflibs(list[lptr],libamt,ldrno) <= libamt)){
            grl = list[lptr];  /* found a group */
            val = 50 + 50 * grsize[grl];  /* value of defense of grl */
	    if(val > 200)val = 200;
            if(grlibs[grl] == 1){
               val = 100 * lnbn[list[grlbp[grl]]];
               if(grsize[grl] > 1){
                  val = 500+100*grsize[grl];
                  }
               }
            for(j = grnbp[grl]; j != EOL; j = links[j])
                if(list[j] != gs){	/* cutting stones */
		   if(grlibs[grl] == 1)val += grlibs[list[j]]*100;
                   else if(grlibs[list[j]] < 5)
                      val += 25 * grlibs[list[j]] - 25;
		   else
		      val += 200;
		   break;
		   }

            for(lptr2 = grnbp[grl]; lptr2 != EOL;
               lptr2 = links[lptr2]){  /* captur nbr of nbr to defend nbr */
               if(grlibs[list[lptr2]] == 1){
                  mvv[mvp] = val + 100;
                  mvl[mvp] = list[grlbp[list[lptr2]]];
                  mvp++;
                  }
               }

		/* move in liberty of group to defend */
		/* only try best move, not all moves */
	    flag = FALSE;
	    mvv[mvp] = 0;
            for(lptr2 = grlbp[grl]; lptr2 != EOL;
               lptr2 = links[lptr2]){ /* play next to grl */
	       cancapture = FALSE; /* does move capture opponent */
	       cpylist(grlbp[grl],&tmplist);
               numlibs = mrglist(nblbp[list[lptr2]],&tmplist)-1;
			 /* number of new liberties from move */
	       nbflag = FALSE;
               j = fdir[list[lptr2]];
               for(ldtmp = ldir[j]; j < ldtmp; ++j){
                     sn = list[lptr2]+nbr[j];
                     if(grcolor[board[sn]] == ctm){
                        if(board[sn] != grl){
			   numlibs += mrglist(grlbp[board[sn]],&tmplist);
                           }
                        }
                     else {
			     if(grlibs[board[sn]] == 1)cancapture = TRUE;
			     nbflag = TRUE;
			     }
                     }
	       if(tmplist != EOL)killist(&tmplist);

	                   /* don't generate moves that opponent wouldn't make
			                     anyway */
               if(!cancapture && (lnbf[list[lptr2]][ctm] == 0 && 
		  lnbn[list[lptr2]] <= 1 || grlibs[grl] + numlibs <= 1) ){
                     continue;
                     }
		libval = val + 10*numlibs + 50*nbflag;
		if(numlibs < 1)libval -= val;
	       if(libval > mvv[mvp]){
		 mvv[mvp] = libval;
		 mvl[mvp] = list[lptr2];
		 flag = TRUE;
		 }

               }
            if(flag)++mvp;
            }
         }
      }
   }

/* find out if a group is alive or dead.
 *
 * group dies if it has one liberty and it is opponents move or
 * group has one liberty and cant get more
 * color is the color of the group being chased
 *
 * group lives if it has more than maxlibs liberties or
 * search is more than maxply deep or
 * search is more than maxnodes wide or
 * group has maxlibs libs and can't be prevented from getting another or
 * 
 */

int livesordies(int starts,int maxlibs,int ctm,int color,int ply,int ldrno){
   int lbs,ptr,ecount,leaf,count,fcount,gcount,g,s;
   int g2,ptr2;
   leaf = FALSE;
   g = board[starts];
   if(grlibs[g] == 1 && color != ctm){  /* dies */
      scrply[ply] = 1;  /* dead */

      return(TRUE);
      }
   lbs = getefflibs(g,maxlibs,ldrno);
   ecount = 0;	/* number of libs with 3 libs */
   if(lbs == maxlibs){
      for(ptr = grlbp[board[starts]]; ptr != EOL; ptr = links[ptr]){
         if(lnbn[list[ptr]] == 3 && 
		newlist(grlbp[board[starts]],nblbp[list[ptr]]) == 3)++ecount;
         }
      }
   count = 0;	/* nbr groups with less libs (attackable) */
   fcount = 0;  /* nbr groups with 1 lib*/
   gcount = 0;  /* biggest nbr 1 lib group */
   if(ctm == color)
    for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
      if(grlibs[list[ptr]] <= grlibs[g]){
	      for(ptr2 = grlbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2])
		      if(lnbn[list[ptr2]] > 1){
			      ++count;
			      break;
			      }
	      }
      if(grlibs[list[ptr]] == 1){
         ++fcount;
         if(grsize[list[ptr]] > gcount)gcount = grsize[list[ptr]];
         }
      }
   s = list[grlbp[g]];
   if(grlibs[g] == 1 && fcount == 0){   /* one lib group no one lib nbrs */
      if(lnbn[s] < 2){   /* cant get more libs */
         if(lnbf[s][color] == 1){
            scrply[ply] = -1;  /* dead */
            return(TRUE);
            }
         }
      for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
         for(ptr2 = grnbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
            g2 = list[ptr];
            if(g2 != g && grlibs[g2] == 1 && grsize[g2] > grsize[g]){
               scrply[ply] = -1;  /* dead */

               return(TRUE);
               }
            }
         }
      }      

   if( ecount > (ctm != color) ||  /* can't stop from getting too many libs*/
      lbs > maxlibs || /* has too many libs */
      count > 2 || /* too many nbrs can be attacked */
      fcount > 2 || /* too many nbrs can be captured */
      gcount > 3 ){ /* too big a nbr can be captured */
         leaf = TRUE;  /* lives or dies, search complete here */
         scrply[ply] = -1;
         if(color == ctm)scrply[ply] = 1;
         }
   return(leaf);
   }


  
