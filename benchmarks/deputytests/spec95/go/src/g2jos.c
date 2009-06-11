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
# include "g2rldef.h"
  
extern int jlib2[NUMJOSBYTES],xymap[62];

/* NOTE: Originally, it was foundjoseki[4], Raymond changed it to [5] */

extern int cfac[],foundjoseki[5],passval;
void getxyjlib(int ptr,int *x,int *y);



/* jupdate updates jptr, jptr2, jreflect, jcolor for all corners when 
 * a move at s, color c is made
 */

void jupdate(int s,int c){
	int corner;
   if(s == PASS)return;
   for(corner = 0; corner < 4; ++corner)
   	   jupdatec(s,c,corner);
	}

void jupdatec(int s,int c,int corner){
   int x,y,xtmp,ytmp,tmp,mid,ptr,found,lastcolor,ptr2,inacorner;

       mid = (boardsize+1)/2;
      	x = xval[s] + 1;
      	y = yval[s] + 1;
        if(corner > 1)
                  y = boardsize+1-y;
        if(corner%2 == 1)
                  x = boardsize+1-x;
	inacorner = x <= mid+1 && y <= mid+1 && x+y < mid*2-2;
	if(x > 14 || y > 14 || x+y > 10)return;

	if(jreflect[corner] == 1){
		tmp = x;
		x = y;
		y = tmp;
		}


	if(jptr2[corner] == 0){  /* first move in corner */
		lastcolor = 1-c;
		jcolor[corner] = c;
		}
	else
		lastcolor = getlastcolor(corner);

	if(lastcolor == NOCOLOR){
		if(jflag[corner] < 20)jflag[corner]++;
		return;
		}

	found = FALSE;
	for(ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)){
		getxyjlib(ptr,&xtmp,&ytmp);
		if(xtmp == 0){
			if(lastcolor != c)continue;
			ptr2 = j2next(ptr);
			getxyjlib(ptr2,&xtmp,&ytmp);
			if(x == 0)continue;
			if(x == xtmp && y == ytmp){
				jptr2[corner] = ptr2;
				if(jreflect[corner] == 0 && x != y)
					jreflect[corner] = 2;
				found = TRUE;
				break;
				}
			if(jreflect[corner] == 0 &&
			   x == ytmp && y == xtmp){
				jptr2[corner] = ptr2;
				if(x != y)
					jreflect[corner] = 1;
				found = TRUE;
				break;
				}
			continue;
			}
				
		if(lastcolor == c)continue;
		if(x == xtmp && y == ytmp){
			jptr2[corner] = ptr;
			if(jreflect[corner] == 0 && x != y)
				jreflect[corner] = 2;
			found = TRUE;
			break;
			}
		if(jreflect[corner] == 0 &&
		   x == ytmp && y == xtmp){
			jptr2[corner] = ptr;
			if(x != y)
				jreflect[corner] = 1;
			found = TRUE;
			break;
			}
		}


	if(!found && inacorner){
		if(jflag[corner] < 20)jflag[corner]++;
		}
	else {
		if(getflag(jptr2[corner]) & 0x40)jreflect[corner] = 0;
		}
	}


/* get the color of the last move in the corner */

int getlastcolor(int corner){
	int xtmp,ytmp,tmp,ptr,c;
	if(jptr2[corner] == 0)return(NOCOLOR);
	getxyjlib(jptr2[corner],&xtmp,&ytmp);
		
	if(jreflect[corner] == 1){
		tmp = xtmp;
		xtmp = ytmp;
		ytmp = tmp;
		}
	
	--xtmp; --ytmp;
	if(corner > 1)ytmp = boardsize-ytmp-1;
	if(corner == 1 || corner == 3)xtmp = boardsize-xtmp-1;
	c = grcolor[board[xtmp+boardsize*ytmp]];
	ptr = jptr2[corner];
	if((jlib2[ptr] & 0xc0) == 0xc0){
		tmp = ptr;
		if((jlib2[ptr] & 0x3f) == 63)ptr++;
		if(jlib2[ptr+1] & 0x20){
			if(jlib2[ptr+2] & 0x80)c = 1-c;  /* color reversal */
			if((jlib2[tmp] & 0x3f) == 62)c = 1-c;
			}
		}
	return(c);
	}

int getflag(int ptr){
	if((jlib2[ptr] & 0xc0) != 0xc0)return(0);
	if((jlib2[ptr] & 0x3f) == 63)return(jlib2[ptr+2]);
	return(jlib2[ptr+1]);
	}

/* return the x and y values for this move in j2lib.  return 0,0 for
 * tenuki range is 1-15 for x and y.  
 */


void getxyjlib(int ptr,int* x,int* y){
	int tmp;
	tmp = jlib2[ptr] & 0x3f;
	if(tmp < 62){
		*x = (xymap[tmp] >> 4) & 0xf;
		*y = xymap[tmp] & 0xf;
		}
	else if(tmp == 63){
		*x = (jlib2[ptr+1] >> 4) & 0xf;
		*y = jlib2[ptr+1] & 0xf;
		}
	else {
		*x = 0; *y = 0;
		return;
		}
	return;
	}

/* return index of first response to ptr.  return 0
 * if there is none
 */

int j2next(int ptr){
	int tmp;
	tmp = jlib2[ptr];
	if((tmp & 0xc0) == 0x40)return(0);
	if((tmp & 0x3f) == 63)ptr++;
	if((tmp & 0xc0) != 0xc0)return(ptr+1);
	++ptr;
	tmp = jlib2[ptr];
	if(tmp & 0x10)return(0);
	if(tmp & 0x20){
		tmp = (jlib2[ptr+1] & 0x3f ) << 8;
		tmp |= ((int)jlib2[ptr+2] & 0xff);
		return(tmp);
		}
	++ptr;
	return(ptr);
	}

/* return TRUE if this is last move in sequence.
 * update ptr to index of next move in line in array
 */

int j2skip(int* ptr){
	int tmp;
	tmp = jlib2[*ptr];
	(*ptr)++;
	if((tmp & 0x3f) == 63)(*ptr)++;
	if((tmp & 0xc0) == 0 || (tmp & 0xc0) == 0x80)return(FALSE);
	if((tmp & 0xc0) == 0x40)return(TRUE);
	
	tmp = jlib2[*ptr];
	(*ptr)++;
	if(tmp & 0x20){
		(*ptr) += 2;
		return(TRUE);
		}
	if(tmp & 0x10)return(TRUE);
	return(FALSE);
	}

/* return index of sibling of move at ptr.  return 0 if no
 * sibling
 * old, slow version

int j2more(int ptr){
	int scount = 0,new;
	if(jlib2[ptr] == 62)j2skip(&ptr); 
	if(!sibling(ptr))return(0);
	do {
		if(sibling(ptr))++scount;
		if(j2skip(&ptr))--scount;
		} while(scount > 0);
	return(ptr);
	}

*/

int j2more(int ptr){
	int scount = 0,tmp;
	if(jlib2[ptr] == 62)j2skip(&ptr); 
	if(!sibling(ptr))return(0);
	do {
		tmp = jlib2[ptr++];
		if((tmp & 0x3f) == 63)ptr++;
		if((tmp & 0xc0) == 0x80)++scount;
		else if((tmp & 0xc0) == 0x40)--scount;
		else if((tmp & 0xc0) == 0xc0){
			tmp = jlib2[ptr++];
			if(tmp & 0x20){
				ptr += 2;
				--scount;
				}
			else if(tmp & 0x10)--scount;
			if(tmp & 0x80)++scount;
			}
		} while(scount > 0);
	return(ptr);
	}


/* return TRUE if move at ptr has sibling (only called from j2more) */

int sibling(int ptr){
	int tmp;
	if(jlib2[ptr] == 62)ptr++;
	tmp = jlib2[ptr];
	if(!(tmp & 0x80))return(FALSE);
	if((tmp & 0xc0) == 0x80)return(TRUE);
	if((tmp & 0x3f) == 63)ptr++;
	return(jlib2[ptr+1] & 0x80);
	}




int urgjoscorner[4];

/* joseki finds all joseki moves on board and fires the
 * appropriate strategic rule for that joseki
 * return TRUE if found a NORM or URGENT Joseki move
 */

void joseki(void){
   int corner,ptr,x,y,tmp,cn,lastcolor,xyptr,urgjos = FALSE;
   int bsz,cflag,type,i,xtmp,ytmp;
   for(i = 0; i < 4; ++i)foundjoseki[i] = FALSE;
   for(i = 0; i < 4; ++i)urgjoscorner[i] = FALSE;
   for(ptr = urgdefarmies; ptr != EOL; ptr = links[ptr])
	   urgjoscorner[which_corner(mvs[grpieces[list[armygroups[list[ptr]]]]])] = TRUE;
   if(msptr > boardsquare/2)return;
   if(boardsize < 13)return;

   urgjos = findurgentjoseki();  /* see if there is an urgent joseki */

   bsz = boardsize/2 + 2;
   if(msptr == 0)cn = -1;
   else
	   cn = which_corner(mvs[msptr-1]);
   for(corner = 0; corner < 4; ++corner){
	 if(jflag[corner] > 4)continue;
	 if(jptr2[corner] == 0)continue;  /* let empty_corner make move */
	 if(urgjos && !urgjoscorner[corner]){
		 if(jflag[corner] == 1)foundjoseki[corner] = TRUE;
		 continue;  /* for performance */
		 }
	 lastcolor = getlastcolor(corner);
         for(ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)){
	       type = getflag(jptr2[corner]) & 0xf;
	       cflag = cn == corner && (type == PINC || type == NORM || type == URGT || type == BAD || type == FRKA || type == FOLL || type == COMP || type == TRIK);
	       xyptr = ptr;
	       getxyjlib(xyptr,&x,&y);
	       if(x == 0){
			  if(lastcolor != tm)continue;
			  xyptr = j2next(ptr);
			  getxyjlib(xyptr,&x,&y);
			  if(x == 0)continue;
			  }
	       else if(lastcolor == tm)continue;
	       x--;y--;
	       if(x > bsz || y > bsz)continue;
	       type = getflag(xyptr) & 0xf;
	       if(type == URGT)continue;  /* handled by findurgentjoseki() */
               if(jreflect[corner] <= 1){
                  tmp = x;
                  x = y;
                  y = tmp;
                  }
	       xtmp = x;
	       ytmp = y;
               if(corner > 1)
                  ytmp = boardsize-1-ytmp;
               if(corner%2 == 1)
                  xtmp = boardsize-1-xtmp;
	       
	       fire_joseki(xyptr,ytmp*boardsize+xtmp,jflag[corner],cflag,corner);

               if(jreflect[corner] == 0 && x != y){
	          getxyjlib(xyptr,&x,&y);
		  x--;y--;
                  if(corner > 1)
                     y = boardsize-1-y;
                  if(corner%2 == 1)
                     x = boardsize-1-x;
                  fire_joseki(xyptr,y*boardsize+x,jflag[corner],cflag,corner);
                  }
            }
        }

   return;
   }



int findurgentjoseki(void){
   int bsz,x,y,tmp,corner,ptr,type,urg = FALSE,lastcolor,xyptr;

   bsz = (boardsize+1)/2;
   for(corner = 0; corner < 4; ++corner){
	 if(jflag[corner] > 4)continue;
	 if(jptr2[corner] == 0)lastcolor = 1-tm;
	 else
		 lastcolor = getlastcolor(corner);
         for(ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)){
	       xyptr = ptr;
	       getxyjlib(xyptr,&x,&y);
	       if(x == 0){
			  if(lastcolor != tm)continue;
			  xyptr = j2next(ptr);
			  getxyjlib(xyptr,&x,&y);
			  if(x == 0)continue;
			  }
	       else if(lastcolor == tm)continue;
	       x--;y--;
	       if(x > bsz || y > bsz)continue;
	       type = getflag(xyptr) & 0xf;
	       if(type != URGT)continue;

	       urg = TRUE;
               if(jreflect[corner] <= 1){
                  tmp = x;
                  x = y;
                  y = tmp;
                  }
               if(corner > 1)
                  y = boardsize-1-y;
               if(corner%2 == 1)
                  x = boardsize-1-x;
	       fire_joseki(xyptr,y*boardsize+x,jflag[corner],FALSE,corner);

               if(jreflect[corner] == 0 && x != y){
	          getxyjlib(xyptr,&x,&y);
		  x--;y--;
                  if(corner > 1)
                     y = boardsize-1-y;
                  if(corner%2 == 1)
                     x = boardsize-1-x;
                  fire_joseki(xyptr,y*boardsize+x,jflag[corner],FALSE,corner);
                  }
            }
	 }
   if(boardsize != 19)urg = FALSE;  /* may not be urgent on small boards */
   return(urg);
   }

void fire_joseki(int ptr,int s,int jflg,int cflag,int corner){
	int val,type,scrtmp;
	if(board[s] != NOGROUP)return;
	type = getflag(ptr) & 0xf;
	if(jflg != 1 && type != BAD && 
	   type != IGNR && board[s] == NOGROUP)
		fire_strat_rule(s,MAYBE_JOSEKI,0,NOSQUARE,NOT_URGENT);
	if(jflg != 1 && jflg != 2)return;
	if(type == FRFR)val = 400;
	else val = 0;

	switch(type){
	      case FRKA:
		fire_strat_rule(s,JOSEKI,val,NOSQUARE,NOT_URGENT);
		if(jflg == 1)foundjoseki[corner] = TRUE;
		break;
	      case PINC:
		fire_strat_score(s,JOSEKI,21*50,NOT_URGENT);
		if(jflg == 1)foundjoseki[corner] = TRUE;
		if(cornercolor(pincerstonefacing(corner)) == tm)
		fire_strat_score(s,PINCER_EXTENSION,100,NOT_URGENT);
		break; 
	      case NORM:
		if(jflg == 1){
			foundjoseki[corner] = TRUE;
			if(playlevel > 15 && (urgdefarmies == EOL || urgjoscorner[corner])){
				scrtmp = evaljoseki(s,ptr,corner) - passval;
				life();
				getscore();
				scrtmp += 500;
				if(scrtmp < 250)scrtmp = 250;
				if(scrtmp > 17*50)
					scrtmp = 17*50;
	        	fire_strat_score(s,JOSEKI,scrtmp,urgjoscorner[corner]);
				if(cflag)
					fire_strat_score(s,JOSEKI_NO_TENUKI,500,NOT_URGENT);
				}
			else {
	        	fire_strat_rule(s,JOSEKI,val,NOSQUARE,urgjoscorner[corner]);
				 if(cflag)
					fire_strat_rule(s,JOSEKI_NO_TENUKI,0,NOSQUARE,NOT_URGENT);
				}
			}
		break;
	      case URGT:
		if(jflg == 1){
			fire_strat_score(s,URG_JOSEKI,1200,URGENT);
			foundjoseki[corner] = TRUE;
			if(cflag){
				fire_strat_score(s,JOSEKI_NO_TENUKI,500,NOT_URGENT);
				}
			}
		break;
	      case COMP:
		if(ahead >= 2)break;
		fire_strat_rule(s,COMP_JOSEKI,0,NOSQUARE,NOT_URGENT);
		break;
	      case BAD:
		fire_strat_rule(s,BAD_JOSEKI,0,NOSQUARE,NOT_URGENT);
		break;
	      case SHIM:
		fire_strat_rule(s,SHIM_JOSEKI,0,NOSQUARE,NOT_URGENT);
		if(jflg == 1)foundjoseki[corner] = TRUE;
		break;
	      case FOLL:
		fire_strat_rule(s,FOLL_JOSEKI,0,NOSQUARE,NOT_URGENT);
		break;
	      case TRIK:
		if(ahead >= 2)break;
		fire_strat_rule(s,TRIK_JOSEKI,0,NOSQUARE,NOT_URGENT);
		break;
		}
	}

/* return TRUE if s doesn't have any joseki reasons associated with it */

int notjoseki(int s){
	int ptr,r;
	for(ptr = stratreasons[s]; ptr != EOL; ptr = links[ptr]){
		r = strat[list[ptr]].reason;
		if(r == JOSEKI || r == TRIK_JOSEKI || r == FOLL_JOSEKI ||
		   r == COMP_JOSEKI || r == URG_JOSEKI || r == SHIM_JOSEKI)
			return(FALSE);
		}
	return(TRUE);
	}

/* evaluate a joseki starting with a move at s. ptr is index of this move.
 * it is in corner.  return score with sign that more positive is better
 */



int scccl[] = { -1, 1, 0};

int evaljoseki(int s,int ptr,int corner){

	int type,x,y,score,stmp,tmp,lastcolor,xyptr;
	int oldjcolor[4],oldjreflect[4],oldjflag[4];
	int oldjptr[4],cn;
	for(cn = 0; cn < 4; ++cn){
		oldjcolor[cn] = jcolor[cn];
		oldjreflect[cn] = jreflect[cn];
		oldjptr[cn] = jptr2[cn];
		oldjflag[cn] = jflag[cn];
		}
	if(board[s] != NOGROUP){  /* already stone where you want to play */
		life();
		score = getscore()*scccl[tm];
		if(grcolor[board[s]] == tm)score += 250;
		else score -= 250;
		for(cn = 0; cn < 4; ++cn){
			jptr2[cn] = oldjptr[cn];
			jcolor[cn] = oldjcolor[cn];
			jreflect[cn] = oldjreflect[cn];
			jflag[cn] = oldjflag[cn];
			}
		return(score);
		}
	mvs[msptr] = s;
	mvcolor[msptr] = tm;
	update(msptr,TRUE);
	jupdate(s,tm);
	++msptr;
	tm = 1-tm;
	score = BIGNUM;
	if(jptr2[corner] == 0)lastcolor = 1-tm;
	else
		lastcolor = getlastcolor(corner);
	for(ptr = j2next(jptr2[corner]); ptr != 0; ptr = j2more(ptr)){

		xyptr = ptr;
		getxyjlib(xyptr,&x,&y);
		if(x == 0){
			if(lastcolor != tm)continue;
			xyptr = j2next(ptr);
			getxyjlib(xyptr,&x,&y);
			if(x == 0)continue;
			}
		else if(lastcolor == tm)continue;
		x--;y--;
		type = getflag(ptr) & 0xf;
		if(type != NORM && type != URGT)continue;
		if(jreflect[corner] <= 1){
			tmp = x;
			x = y;
			y = tmp;
			}
		if(corner > 1)
			y = boardsize-1-y;
		if(corner%2 == 1)
			x = boardsize-1-x;
		stmp = -evaljoseki(x+boardsize*y,xyptr,corner);
		if(stmp < score)score = stmp;
		}
	if(score == BIGNUM){  /* no more moves */
		life();
		score = getscore()*scccl[tm];

		--msptr;
		dndate(msptr);
		tm = mvcolor[msptr];
		for(cn = 0; cn < 4; ++cn){
			jptr2[cn] = oldjptr[cn];
			jcolor[cn] = oldjcolor[cn];
			jreflect[cn] = oldjreflect[cn];
			jflag[cn] = oldjflag[cn];
			}
		return(score);
		}
	--msptr;
	dndate(msptr);
	for(cn = 0; cn < 4; ++cn){
		jptr2[cn] = oldjptr[cn];
		jcolor[cn] = oldjcolor[cn];
		jreflect[cn] = oldjreflect[cn];
		jflag[cn] = oldjflag[cn];
		}
	tm = mvcolor[msptr];
	return(score);
	}
