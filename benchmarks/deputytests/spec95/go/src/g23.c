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
# include "g2rldef.h" 

extern int sumpots[];
extern int ltrfac[],pfac[],defv[],atkv[],kval[],ctval[],connval[],sumeyes[],thalive[];
extern int dirnm[],opdir[],msks[];
extern struct shapestruct shapes[];  

int passval;  /* value of a pass */
int foundjoseki[5];  /* joseki() found a joseki or kakari or shimari move by corner */
  
/* add a rule to a square.  Rule rule fired for square s with
 * additional value val.  param is a value to pass on check if rule satisfied
 */

void fire_strat_rule(int s,int rule,int val,int param,int urg){

	if(nextstrat == NUMSTRATS){
		return;
		}
	strat[nextstrat].reason = rule;
	strat[nextstrat].value = val;
	strat[nextstrat].goodrule = FALSE;
	strat[nextstrat].param = param;
	strat[nextstrat].urgent = urg;
	if(urg)urgent[s]++;
	stratguess[s] += val + rules[rule].guess;
	adflist(nextstrat,&stratreasons[s]);
	nextstrat++;
	}

/* add a rule to a square.  Rule rule fired for square s with
 * actual val.  param is a value to pass on check if rule satisfied
 */

	
void fire_strat_score(int s,int rule,int val,int urg){

	if(nextstrat == NUMSTRATS){
		return;
		}
	strat[nextstrat].reason = rule;
	strat[nextstrat].value = val;
	strat[nextstrat].goodrule = TRUE;
	strat[nextstrat].param = NOSQUARE;
	strat[nextstrat].urgent = urg;
	if(urg)urgent[s]++;
	stratguess[s] += val + rules[rule].guess;
	adflist(nextstrat,&stratreasons[s]);
	nextstrat++;
	if(scoreval[s] == BIGNUM)scoreval[s] = 0;
	scoreval[s] += val;
	}



/* has rule fired for square s?
 */

int
rule_fired(int s,int rule){
	int ptr;
	for(ptr = stratreasons[s]; ptr != EOL; ptr = links[ptr])
		if(strat[list[ptr]].reason == rule)return(TRUE);
	return(FALSE);
	}


int cfac[2] = { -1,1 };
int dfac[19] = { 1,1,2,4,8,8,8,8,8,0 };
  

/* corner pointed at */
  
int point_corner[2][4] = {
		{ 1,0,3,2 },
		{ 2,3,0,1 }
			};

int empty_corner(void){
   int corner,numempties,s;

   if(msptr == 0){	
      if(boardsize == 7){
         s = 24;  /* center point for 7 line boards */
	 fire_strat_score(s,CENTER_FOR_7_LINE,1000,URGENT);
	 return(TRUE);
    	 }
     }


   numempties = 0;
   for(corner = 0; corner < 4; ++corner)
      if(emptycorner(corner))	/* found mt corner */
         ++numempties;
   if(numempties == 0)return(FALSE);

/* rules: play in an empty corner
 *	play on 3-3, 3-4, or 4-4 point only
 *	make 3-4 point plays spiral (don't face 3-4 points at each other)
 *	play in corner adjacent to 4-4 point stone if >1 empty corners
 *	play in corner opponent is facing on 3-4 point
 *	play 3-4 point facing empty corner
 * 	don't play on 3-4 point facing opponent
 *		unless there are 2 empty corners 
 *	don't face two 3-4 points at same empty corner
 */

   for(corner = 0; corner < 4; ++corner)
	   if(emptycorner(corner)){	/* found mt corner */
		   threethreecorner(corner,numempties);
		   threefourcorner(corner,numempties);
		   if(boardsize <=13)continue;
		   fourfourcorner(corner,numempties);
		   }
   return(TRUE);
   }


void threethreecorner(int corner,int numempties){
	int s;
	s = threethreepoint(corner);
	fire_strat_score(s,PLAY_IN_EMPTY_CORNER,1000,URGENT);
	if(numempties == 3 || numempties == 1)scoreval[s] += 100;
	if(stonefacing(point_corner[0][corner]) == corner &&
	   cornercolor(point_corner[0][corner]) == 1-tm){
		fire_strat_score(s,PLAY_IN_CORNER_POINT,100,NOT_URGENT);
		}
	if(stonefacing(point_corner[1][corner]) == corner &&
	   cornercolor(point_corner[1][corner]) == 1-tm){
		fire_strat_score(s,PLAY_IN_CORNER_POINT,100,NOT_URGENT);
		}
	}


void threefourcorner(int corner,int numempties){
	int s,dir;
	for(dir = 0; dir < 2; ++dir){

		s = threefourpoint(corner,dir);	/* move under consideration */
		/*	fire_strat_rule(s,BOARD_NOT_TOO_SMALL,0,NOSQUARE); ,NOT_URGENT*/
		fire_strat_score(s,PLAY_IN_EMPTY_CORNER,1100,URGENT);

		if(spiral(corner,dir)){   /* make 3-4 points spiral */
			fire_strat_score(s,MAKE_34_POINTS_SPIRAL,300,NOT_URGENT);
			}

		if(numempties > 1 && (stoneon44point(point_corner[0][corner]) ||
				      stoneon44point(point_corner[1][corner]))){ 
				/* corner adjacent to 4-4 point stone */
			fire_strat_score(s,PLAY_NEXT_TO_44_POINT_STONE,100,NOT_URGENT);
			}

		if(stonefacing(point_corner[0][corner]) == corner &&
		   cornercolor(point_corner[0][corner]) == 1-tm){
			fire_strat_score(s,PLAY_IN_CORNER_POINT,100,NOT_URGENT);
			}
		
		if(stonefacing(point_corner[1][corner]) == corner &&
		   cornercolor(point_corner[1][corner]) == 1-tm){
			fire_strat_score(s,PLAY_IN_CORNER_POINT,100,NOT_URGENT);
			}
		
		if(emptycorner(point_corner[dir][corner])){
			fire_strat_score(s,POINT_AT_EMPTY,100,NOT_URGENT);
			/* point 3-4 point at empty corner */
			}
		
		if(stoneon44point(point_corner[dir][corner]) && 
		   cornercolor(point_corner[dir][corner]) == tm){
			fire_strat_score(s,POINT_AT_FRIENDLY_44,100,NOT_URGENT);
			scoreval[s] += 100;
			}
		
		if(cornercolor(point_corner[dir][corner]) == 1-tm){ 
			/* 3-4 facing opponent */
			if(numempties != 2){
				fire_strat_score(s,DONT_POINT_AT_OPP,-300,NOT_URGENT);
				}
			
			}
		}
	}


void fourfourcorner(int corner,int numempties){
	int s;
	s = fourfourpoint(corner);
	fire_strat_score(s,PLAY_IN_EMPTY_CORNER,1100,URGENT);
	if(numempties > 1 && (stoneon44point(point_corner[0][corner]) ||
			      stoneon44point(point_corner[1][corner]))){ 
		/* corner adjacent to 4-4 point stone */
		fire_strat_score(s,PLAY_NEXT_TO_44_POINT_STONE,100,NOT_URGENT);
		scoreval[s] += 100;
		}
	if(stoneon44point(point_corner[0][corner]) && 
	   cornercolor(point_corner[0][corner]) == tm ||
	   stoneon44point(point_corner[1][corner]) && 
	   cornercolor(point_corner[1][corner]) == tm){
		fire_strat_score(s,NI_REN_SEI,200,NOT_URGENT);
		}
	if(emptycorner(point_corner[0][corner]) && emptycorner(point_corner[1][corner])){
		fire_strat_score(s,PLAY_44_2_EMPTY,100,NOT_URGENT);
		}
	if(stonefacing(point_corner[0][corner]) == corner &&
	   cornercolor(point_corner[0][corner]) == 1-tm){
		fire_strat_score(s,PLAY_IN_CORNER_POINT,200,NOT_URGENT);
		}
	if(stonefacing(point_corner[1][corner]) == corner &&
	   cornercolor(point_corner[1][corner]) == 1-tm){
		fire_strat_score(s,PLAY_IN_CORNER_POINT,200,NOT_URGENT);
		}
	}




/* the following routines deal with corners of the board.  corners are
 * numbered :
 *               0   1
 *
 *               2   3
 */

/* return true if stone on 4-4 point in corner and no other stones */

int stoneon44point(int corner){
	return(jflag[corner] == 1 && (getflag(jptr2[corner]) & 0xf) == FRFR);
	}

/* return true if stone on 3-4 point in corner and no other stones */

int stoneon34point(int corner){
	return(jflag[corner] == 1 && (getflag(jptr2[corner]) & 0xf) == THFR);
	}

/* return TRUE if stone on 5-4 or 5-3 point in corner and no other stones */

int stoneonhighpoint(int corner){
	return(jflag[corner] == 1 && (getflag(jptr2[corner]) & 0xf) == HIGH);
	}



/* return color of first stone in corner */

int cornercolor(int corner){
	if(corner == -1)return(NOCOLOR);
	if(jptr2[corner] == 0)return(NOCOLOR);
	return(jcolor[corner]);
	}

/* return corner number stone on 3-4 point is facing in corner */

int stonefacing(int corner){
	int x,y,sqr;
	if(!stoneon34point(corner))return(-1);
	x = 2;
	y = 3;
	sqr = reflect(x,y,corner);
	if(board[sqr] != NOGROUP)return(point_corner[0][corner]);
	else return(point_corner[1][corner]);
	}

int pincerstonefacing(int corner){
	int x,y,sqr;
	x = 2;
	y = 3;
	sqr = reflect(x,y,corner);
	if(board[sqr] != NOGROUP)return(point_corner[0][corner]);
	else return(point_corner[1][corner]);
	}

/* return square of high approach to 3-4 point in corner */

int highpoint(int corner){
	int x,y,sqr;
	if(!stoneon34point(corner))return(-1);
	x = 2;
	y = 3;
	sqr = reflect(x,y,corner);
	if(board[sqr] != NOGROUP){
		x = 4;
		y = 3;
		return(reflect(x,y,corner));
		}
	x = 3;
	y = 4;
	return(reflect(x,y,corner));
	}


/* return square of low approach to 3-4 point in corner */

int lowpoint(int corner){
	int x,y,sqr;
	if(!stoneon34point(corner))return(-1);
	x = 2;
	y = 3;
	sqr = reflect(x,y,corner);
	if(board[sqr] != NOGROUP){
		x = 4;
		y = 2;
		return(reflect(x,y,corner));
		}
	x = 2;
	y = 4;
	return(reflect(x,y,corner));
	}

/* true if 3-4 in corner facing dir makes spiral */

int spiral(int corner,int dir){
	int cn;

	cn = point_corner[dir][corner];
	if(stoneon34point(cn) && cornercolor(cn) == tm && 
		stonefacing(cn) != corner)
		return(TRUE);

	cn = point_corner[1-dir][corner];
	if(cornercolor(cn) == tm && stonefacing(cn) == corner)
		return(TRUE);

	return(FALSE);
	}

/* true if corner is empty */

int emptycorner(int corner){
      if(jflag[corner] == 1 && jptr2[corner] == 0)return(TRUE);
	return(FALSE);
	}


/* return square number of 4-4 point in corner */

int fourfourpoint(int corner){
	int x,y;
	x = 3;
	y = 3;
	return(reflect(x,y,corner));
	}

/* return square number of 3-4 point facing dir in corner */

int threefourpoint(int corner,int dir){
	int x,y;
	x = 2;
	y = 3;
	if(dir == 1){
		x = 3;
		y = 2;
		}
	return(reflect(x,y,corner));
	}


/* return square number of x,y reflexted into corner */

int reflect(int x,int y,int corner){
         if(corner > 1)
            y = boardsize-1-y;
         if(corner == 1 || corner == 3)
            x = boardsize-1-x;
	 return(y*boardsize+x);
	 }

/* return which corner a square is in */
int which_corner(int s){
	int cn = 0;
	if(s == PASS || s == NOSQUARE)return(NOCORNER);
	if(xval[s] * 2 > boardsize)cn += 1;
	if(yval[s] * 2 > boardsize)cn += 2;
	return(cn);
	}


void shimari_kakari(void){
	int numshimari[2],num34points[2],corner,numhighpoints[2];

/* rules:
 *	don't let opponent make 2 shimari
 *	make shimari 
 *	make kakari
 */

	if(boardsize < 13)return;
	numshimari[0] = numshimari[1] = 0;
	num34points[0] = num34points[1] = 0;
	numhighpoints[0] = numhighpoints[1] = 0;
	for(corner = 0; corner < 4; corner++){
		if(is_shimari(corner))numshimari[cornercolor(corner)]++;
		if(stoneon34point(corner))num34points[cornercolor(corner)]++;
		if(stoneonhighpoint(corner))numhighpoints[cornercolor(corner)]++;
		}


	if(numshimari[1-tm] == 1 && (num34points[1-tm] >= 1 || numhighpoints[1-tm] >= 1))
		makekakari(TRUE);


	else if(num34points[tm] >= 1){
		makeshimari();
		if(num34points[1-tm] >= 1 || numhighpoints[1-tm] >= 1)
			makekakari(FALSE);
		return;
		}

	else if(num34points[1-tm] >= 1 || numhighpoints[1-tm] >= 1)
		makekakari(FALSE);
	}


/* true if corner has shimari in it */

int is_shimari(int corner){
	return(jflag[corner] && (getflag(jptr2[corner]) & 0xf) == SHIM);
	}


/* make the best kakari. */

void makekakari(int prevent2shimari){
	int corner,cn,stmp,ptr,count;

/* Joseki will show the possible kakari moves.
 * extensions will find low kakaris from friendly stones.
 * rules:
 *	prevent 2 shimaris
 *	if facing corner is 4-4 point use high kakari
 *	if facing corner has enemy shimari use high kakari
 *	else use low shimari
 *      prefer kakari from friendly corner
 */
	for(corner = 0; corner < 4; ++corner){
		if(cornercolor(corner) == 1-tm && stoneonhighpoint(corner)){
			stmp = threefourpoint(corner,0);
			count = 0;
			for(ptr = nblbp[stmp]; ptr != EOL; ptr = links[ptr])
				if(lnbn[list[ptr]] < 4)count++;
			if(count > 1 || lnbn[stmp] < 4)
				stmp = threefourpoint(corner,1);
			fire_strat_score(stmp,KAKARI,18*50,NOT_URGENT);
			foundjoseki[corner] = TRUE;
			if(prevent2shimari){
				fire_strat_score(stmp,PREVENT_2_SHIMARI,250,URGENT);
				}
			}
		if(cornercolor(corner) == 1-tm && stoneon34point(corner)){
			/* found possible kakari */
			stmp = highpoint(corner);
			fire_strat_score(stmp,KAKARI,18*50,NOT_URGENT);
			foundjoseki[corner] = TRUE;
			if(prevent2shimari){
				fire_strat_score(stmp,PREVENT_2_SHIMARI,250,URGENT);
				}
			cn = stonefacing(corner);
			if(stoneon44point(cn) || is_shimari(cn) && 
				cornercolor(cn) == 1-tm){
				/* high kakari */
				stmp = highpoint(corner);
				fire_strat_score(stmp,SHIMARI_OR_44_POINT,100,NOT_URGENT);
				}
			stmp = lowpoint(corner);
			fire_strat_score(stmp,KAKARI,18*50,NOT_URGENT);
			foundjoseki[corner] = TRUE;
			if(prevent2shimari){
				fire_strat_score(stmp,PREVENT_2_SHIMARI,250,URGENT);
				}
			cn = stonefacing(corner);
			if(cornercolor(cn) == tm){  /* kakari is extension */
				fire_strat_score(stmp,KAKARI_EXTENSION,200,URGENT);
				}
			}
		}
	}

/* make the best shimari.  return TRUE if urgent */

void makeshimari(void){
	int corner,s,cn;

/* rules:
 *	best shimari is on 3-4 point facing enemy corner
 *	make small knights shimari
 */
	for(corner = 0; corner < 4; ++corner){
		if(cornercolor(corner) == tm && stoneon34point(corner)){
			s = lowpoint(corner);
			fire_strat_score(s,MAKE_SHIMARI,15*50,NOT_URGENT);
			foundjoseki[corner] = TRUE;

			cn = stonefacing(corner);
			if(cornercolor(cn) == 1-tm){
				fire_strat_score(s,STONE_FACING,9*50,URGENT);
				}
			}
		}
	}


/* s is the edge of extension area.  longincr points into area.
 * shortincr points toward center.  s is on 3 line.  Return TRUE if
 * there is a shimari here
 */

int edge_shimari(int s,int longincr,int shortincr){
	if(board[s] == NOGROUP)return(FALSE);
	if(edge2[s] != 4)return(FALSE);
	if(grcolor[lgr[s+2*shortincr-longincr]] != grcolor[board[s]])
		return(FALSE);
	if(board[s+shortincr] != NOGROUP)return(FALSE);
	if(board[s+2*shortincr] != NOGROUP)return(FALSE);
	if(board[s-shortincr] != NOGROUP)return(FALSE);
	return(TRUE);
	}


/* figure out the end points for an extension along an edge
 * startpoint is point on 3 line at end of edge
 * longincr is the amount to add to move along the 3 line
 * shortincr is the amount to add to get to the 4 line
 */

void extedge(int startpoint,int longincr,int shortincr){
   int s1,s2,s3,s4,s5;  /* follow 1,2,3,4,5 line */
   int i,state;
   int startext; /* point on 1st line where stone is */
   int endext; /* point on 1st line where stone is */
   int shimari; /* number of shimaris facing along this edge */

   state = 0;
   shimari = 0;
   s3 = startpoint;		/* s3 follows 3 line */
   s4 = startpoint + shortincr; /* s4 follows 4 line */
   s2 = startpoint - shortincr; /* s2 follows 2 line */
   s1 = startpoint - 2*shortincr; /* s1 follows 1 line */
   s5 = startpoint + 2*shortincr; /* s5 follows 5 line */
   for(i = 0; i < boardsize-4; ++i){
      if(state == 0){ /* looking for first stones */
         if(board[s1] != NOGROUP || board[s2] != NOGROUP || 
            board[s3] != NOGROUP || 
            board[s4] != NOGROUP || board[s5] != NOGROUP)
           state = 1;
         }
      else if(state == 1){ /* looking for empty area */
         if(board[s1] == NOGROUP && board[s2] == NOGROUP && 
            board[s3] == NOGROUP && 
            board[s4] == NOGROUP && board[s5] == NOGROUP){
            state = 2;
	    if(edge_shimari(s3-longincr,longincr,shortincr))++shimari;
            startext = s1 - longincr;
            }
         }
      else if(state == 2){ /* looking for stones */
         if(board[s1] != NOGROUP || board[s2] != NOGROUP || 
            board[s3] != NOGROUP || 
            board[s4] != NOGROUP || board[s5] != NOGROUP){
            state = 1;
	    if(edge_shimari(s3,-longincr,shortincr))++shimari;
            endext = s1;

            findbestextension(startext,endext,longincr,shortincr,shimari);
	    shimari = 0;
            }
         }
      s1 += longincr;
      s2 += longincr;
      s3 += longincr;  
      s4 += longincr;  
      s5 += longincr;  
      }
   }


int bmval[] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0 };


/* return TRUE if urgent to extend group g */

int urgextend(int g,int s){
	if(gralive[g] <= ALIVE)return(FALSE);
	if(armysize[grarmy[g]] > 2)return(FALSE);
	if(grlibs[g] < 4)return(FALSE);
	if(jflag[which_corner(s)] == 1)return(FALSE);  /* let joseki handle it */
	if(grnbp[g] == EOL)return(FALSE);
	return(TRUE);
	}

/* figure out the best extension point between startext and endext
 * which are points on the first line where there are stones 
 * on the 1,2,3,4 or 5 line.
 */


void findbestextension(int startext,int endext,int longincr,int shortincr,int shimari){
	int i,s,val;
	int startclr,endclr; /* start and end stone colors */
	int dist,s3;
	int starttwo,endtwo; /* is a wall on end */
	int startaliv,endalive,startgr,endgr;
	int spoint,epoint; /* highest point with stone on it */
	int sline,eline;  /* highest line with stone */
	int slow,elow;    /* lowest line with stone on it */
	int defval,urg;

	s = startext;
	startclr = 3;
	starttwo = FALSE;
	slow = elow = 5;
	for(i = 1; i < 6; ++i){   /* look up and find the stone's color */
	        if(board[s] != NOGROUP){
	                sline = i;
	                spoint = s;
			if(slow == 5)slow = i;
			startgr = board[s];
	                if(startclr == 3)startclr = grcolor[startgr];
 	                else if(startclr != grcolor[startgr])startclr = NOCOLOR;
	                else if(startclr != NOCOLOR)starttwo = TRUE;
	                }
	        s += shortincr;
	        }

	s = endext;
	endclr = 3;
	endtwo = FALSE;
	for(i = 1; i < 6; ++i){
	        if(board[s] != NOGROUP){
	                epoint = s;
	                eline = i;
			endgr = board[s];
			if(elow == 5)elow = i;
	                if(endclr == 3)endclr = grcolor[endgr];
	                else if(endclr != grcolor[endgr])endclr = NOCOLOR;
	                else if(endclr != NOCOLOR)endtwo = TRUE;
	                }
	        s += shortincr;
	        }


  	dist = (endext-startext)/longincr - 1;
	if(dist >= 3 && endclr == tm && grarmy[startgr] != grarmy[endgr]){
		s3 = endext-3*longincr+2*shortincr;
		if(grcolor[startgr] == tm)s3 += shortincr;
		if(dist == 3){
			s3 += longincr;
			}
		if(dist >= 5 && endtwo){
			s3 -= longincr;
			}
		defval = def_val(grarmy[endgr]);
		if(moveispoteye(s3,grarmy[endgr]))defval = 0;
		urg = inlist(grarmy[endgr],&urgdefarmies);
		if(!urg)urg = urgextend(endgr,endext);
		if(gralive[endgr] > WEAK)
			fire_strat_rule(s3,EXTEND_WEAK_GROUP,defval,NOSQUARE,urg);
		else if(gralive[endgr] > ALIVE)
			fire_strat_rule(s3,EXTEND_UNSETTLED_GROUP,
				defval,NOSQUARE,urg);
		}
	if(dist >= 3 && startclr == tm && grarmy[startgr] != grarmy[endgr]){
		s3 = startext+3*longincr+2*shortincr;
		if(grcolor[endgr] == tm)s3 += shortincr;
		if(dist == 3){
			s3 -= longincr;
			}
		if(dist >= 5 && starttwo){
			s3 += longincr;
			}
		defval = def_val(grarmy[startgr]);
		if(moveispoteye(s3,grarmy[endgr]))defval = 0;
		urg = inlist(grarmy[startgr],&urgdefarmies);
		if(!urg)urg = urgextend(startgr,endext);
		if(gralive[startgr] > WEAK)
			fire_strat_rule(s3,EXTEND_WEAK_GROUP,defval,NOSQUARE,urg);
		else if(gralive[startgr] > ALIVE)
			fire_strat_rule(s3,EXTEND_UNSETTLED_GROUP,
				defval,NOSQUARE,urg);
		}


	startaliv = gralive[startgr];
	endalive = gralive[endgr];
	if(startclr == tm && endclr == 1-tm)
	   	ext_to_enemy(startext,endext,longincr,shortincr,spoint,epoint,
			sline,eline,shimari);
	else if(startclr == 1-tm && endclr == tm)
	   	ext_to_enemy(endext,startext,-longincr,shortincr,epoint,
			spoint,eline,sline,shimari);

	else if(startclr == 1-tm || endclr == 1-tm){  /* invasion */
	   if( dist >= 9 && edge2[startext] > 3 && edge2[endext] > 3){  
		   /* can extend both ways and no kakari */
		s3 = startext + (1+dist/2)*longincr + 2*shortincr;
		if(dist%2 == 0 && endalive < startaliv){
			s3 -= longincr;
			}
		if(dist%2 == 0){
			fire_strat_rule(s3,INVADE_NEAR_WEAKER_GROUP,0,NOSQUARE,NOT_URGENT);
			}
		fire_strat_rule(s3,INVADE_WITH_ROOM,50*(dist-6),NOSQUARE,NOT_URGENT);
		if(shimari)fire_strat_rule(s3,SHIM_INVADE,0,NOSQUARE,NOT_URGENT);
		
	      	}
	   if(dist >= 6){
		   val = atk_val(grarmy[startgr]) + dist + 
			   cut_val(startgr,endgr)/2;
		   s3 = startext+2*longincr+2*shortincr;
		   if(rterv[s3][tm] > 0)val += 200;
		   if(startaliv >= MIAI && sline < 5 && grcolor[startgr] != tm){
			fire_strat_rule(s3,INVADE_ATTACK,
				val,NOSQUARE,NOT_URGENT);
			if(sline == 3 && dist >= 7)
				fire_strat_rule(s3+longincr,INVADE_ATTACK,
						val,NOSQUARE,NOT_URGENT);
			}
		   else if(sline == 4 && grcolor[startgr] != tm && 
			   ld[startext+2*shortincr] != 0 &&
			   ld[startext+2*shortincr] < 4)
			fire_strat_rule(s3,INVADE_UNDER_4,val,NOSQUARE,NOT_URGENT);

		   val = atk_val(grarmy[endgr]) + dist + 
			   cut_val(startgr,endgr)/2;
		   s3 = endext-2*longincr+2*shortincr;
		   if(rterv[s3][tm] > 0)val += 200;
		   if(endalive >= MIAI && eline < 5 && grcolor[endgr] != tm){
			fire_strat_rule(s3,INVADE_ATTACK,
				val,NOSQUARE,NOT_URGENT);
			if(eline == 3 && dist >= 7)
				fire_strat_rule(s3-longincr,INVADE_ATTACK,
						val,NOSQUARE,NOT_URGENT);
			}
		   else if(eline == 4 && grcolor[endgr] != tm &&
			   ld[endext+2*shortincr] != 0 &&
			   ld[endext+2*shortincr] < 4)
			fire_strat_rule(s3,INVADE_UNDER_4,val,NOSQUARE,NOT_URGENT);
		   }
	   else if(dist >= 3 && slow >= 4 && elow >= 4){
		s3 = startext + (1+dist/2)*longincr+2*shortincr;
		val = atk_val(grarmy[startgr]) + atk_val(grarmy[endgr]) +
			cut_val(startgr,endgr);
		if(rterv[s3][tm] > 0)val += 200;
		if(dist%2 == 0 && endalive < startaliv){
			s3 -= longincr;
			}
		if(dist%2 == 0){
			fire_strat_rule(s3,INVADE_NEAR_WEAKER_GROUP,0,NOSQUARE,NOT_URGENT);
			}
		fire_strat_rule(s3,INVADE_UNDER_4,val,NOSQUARE,NOT_URGENT);
		}
	   else if(dist >= 3 && (armysize[grarmy[endgr]] == 1 && gralive[endgr] > ALIVE && grlibs[endgr] == 4 ||
				 armysize[grarmy[startgr]] == 1 && gralive[startgr] > ALIVE && grlibs[startgr] == 4)){  /* invade behind single stone */
		s3 = startext + (1+dist/2)*longincr+2*shortincr;
		val = atk_val(grarmy[startgr]) + atk_val(grarmy[endgr]) +
			cut_val(startgr,endgr);
		if(rterv[s3][tm] > 0)val += 200;
		if(dist%2 == 0 && endalive < startaliv){
			s3 -= longincr;
			}
		if(dist == 5 && endalive < startaliv){
			s3 -= longincr;
			fire_strat_rule(s3,INVADE_NEAR_WEAKER_GROUP,0,NOSQUARE,NOT_URGENT);
			}
		if(dist == 5 && endalive > startaliv){
			s3 += longincr;
			fire_strat_rule(s3,INVADE_NEAR_WEAKER_GROUP,0,NOSQUARE,NOT_URGENT);
			}
		if(dist%2 == 0){
			fire_strat_rule(s3,INVADE_NEAR_WEAKER_GROUP,0,NOSQUARE,NOT_URGENT);
			}
		fire_strat_rule(s3,INVADE_BEHIND_WEAK,val,NOSQUARE,NOT_URGENT);
		}
	   else if(ahead < 2 && dist > 2) {
		s3 = startext + (1+dist/2)*longincr+2*shortincr;
		val = 0;
		if(rterv[s3][tm] > 0)val += 200;
		fire_strat_rule(s3,INVADE_BEHIND,val,NOSQUARE,NOT_URGENT);
		}
	   else if(ahead < 2 && dist == 2 && (eline == 4 || sline == 4)){
		   s3 = startext + longincr + 2*shortincr;
		   if(lnbf[s3][1-tm] != 0)
			   s3 = endext - longincr + 2 * shortincr;
		   val = 0;
		   if(rterv[s3][tm] > 0)val = 200;
		   if(lnbf[s3][1-tm] == 0)
			   fire_strat_rule(s3,INVADE_BEHIND,val,NOSQUARE,NOT_URGENT);
		   }
	   }
	else if(startclr == tm && endclr == tm){
		/* big move on side between friendly stones */
  	   dist = (endext-startext)/longincr - 1;
	   if(dist >= 3){
	      	s3 = startext + (1+dist/2)*longincr + 2*shortincr;
		if(dist%2 == 0 && endalive < startaliv){
			s3 -= longincr;
			}
	      	if(sline == 3 && eline == 3){  /* 3 line both ends */
			s3 += shortincr;
			fire_strat_rule(s3,PLAY_HIGH_FROM_LOW,0,NOSQUARE,NOT_URGENT);
			if(dist%2 == 0){
				fire_strat_rule(s3,PLAY_NEAR_WEAKER_GROUP,0,NOSQUARE,NOT_URGENT);
				}
			fire_strat_rule(s3,BIG_MOVE,bmval[dist],NOSQUARE,NOT_URGENT);
			}
		else {
			if(endalive > ALIVE || startaliv > ALIVE)
				fire_strat_rule(s3,BIG_MOVE,bmval[dist],NOSQUARE,NOT_URGENT);
			fire_strat_rule(s3+shortincr,BIG_MOVE,bmval[dist],NOSQUARE,NOT_URGENT);
			if(edge[spoint] == 4 && edge[epoint] == 4 && dist == boardsize - 8)
			   fire_strat_rule(s3+shortincr,MAKE_SAN_REN_SEI,0,NOSQUARE,NOT_URGENT);
			if(dist%2 == 0){
				fire_strat_rule(s3+shortincr,PLAY_NEAR_WEAKER_GROUP,0,NOSQUARE,NOT_URGENT);
				}
			}
                }
	   }
	}





/* figure out extension to enemy.  start is our color, end is enemy 
 * startext and endext are points on first line and empty space
 * in between.  startext is friendly side.  spoint and epoint are the
 * highest point with a stone on them.  shimari is number of shimaris at
 * ends of open area.
 * if the end is a 3-4 point, and dist > 3, make a shimari
 */

int toenemy[] = { 0,0,0,100,150,200,200,250,300,300,300,300,300,
                  300,300,300,300,300,300 };

void ext_to_enemy(int startext,int endext,int longincr,int shortincr,int spoint,int epoint,int sline,int eline,int shimari){
	int s3,dist,atkval,startgr,endgr,val;
	endgr = board[epoint];
	startgr = board[spoint];
	s3 = NOSQUARE;
	dist = (endext-startext)/longincr - 1;
	if(dist < 3)return;
	if(sline == 4 && edge[spoint] == 3 && grsize[startgr] == 1 &&
	        grlibs[startgr] == 4 && lnbn[spoint+2*longincr] == 4){
	        fire_strat_rule(spoint + 2*longincr - shortincr,MAKE_SHIMARI,0,NOSQUARE,NOT_URGENT);
		return;
		}
	if(sline == 4 && gralive[startgr] >= 6 && gralive[startgr] <= ALIVE && dist >= 3 && gralive[endgr] <= ALIVE){
		fire_strat_rule(spoint + 2*longincr - shortincr,COVER_WEAKNESS,0,NOSQUARE,NOT_URGENT);
		if(dist >= 6)
		fire_strat_rule(spoint + 3*longincr - shortincr,COVER_WEAKNESS,0,NOSQUARE,NOT_URGENT);
		}
	atkval = atk_val(grarmy[board[epoint]]);
	if(grsize[board[epoint]] == 1 && grlibs[board[epoint]] < 4)
		atkval = 0;  /* don't extend towards contact fight */
	if(gralive[startgr] <= ALIVE){	/* extend from strength */
	   	if(eline == 4 && board[endext+2*shortincr] == NOGROUP){
	      		s3 = endext - 2*longincr + 2*shortincr;
			if(gralive[board[epoint]] > ALIVE)
				fire_strat_rule(s3,EXTEND_TO_4POINT,toenemy[dist] + 50
				+ atkval,NOSQUARE,NOT_URGENT);
			else
				fire_strat_rule(s3,EXTEND_TO_4POINT,atkval + toenemy[dist],NOSQUARE,NOT_URGENT);
			if(edge2[s3] == 5 && edge[epoint] == 3)
				fire_strat_rule(s3,EXTENSION_IS_KAKARI,0,NOSQUARE,URGENT);
	      		}
		else if(gralive[endgr] <= ALIVE){  /* to strength */
			val = toenemy[dist];
			if(sline == 4)val += 100;
	   		s3 = startext + (1+dist/2)*longincr + 2*shortincr;
                        if(grsize[endgr] == 1 && grcnp[endgr] == EOL &&
                                edge2[epoint] > 4)
                                s3 = endext - 2*longincr + 2*shortincr;
			fire_strat_rule(s3,EXTEND_TO_ENEMY,val,NOSQUARE,NOT_URGENT);
			if(shimari == 1)
				fire_strat_rule(s3,SHIMARI_EXTEND,0,NOSQUARE,NOT_URGENT);
			if(shimari == 2)
				fire_strat_rule(s3,TWO_SHIMARI_EXTEND,0,NOSQUARE,NOT_URGENT);
	      		}
		else if(gralive[endgr] <= UNSETTLED){
			s3 = endext-2*longincr+2*shortincr;
			if(dist > 6 && ld[s3+longincr] > 2 && ld[s3]+longincr != NOLD)
				s3 -= longincr;
			fire_strat_rule(s3,EXTEND_ATTACK_UNSETTLED,toenemy[dist] +
				atkval,NOSQUARE,NOT_URGENT);
			}
		else if(gralive[endgr] <= WEAK){
			s3 = endext-2*longincr+2*shortincr;
			fire_strat_rule(s3,EXTEND_AND_ATTACK,toenemy[dist] +
				atkval,NOSQUARE,NOT_URGENT);
			}
		}
	else if(gralive[startgr] <= UNSETTLED && dist > 2){
/*		defval = def_val(grarmy[startgr]); */
		if(sline <= 3 && dist >= 4){
			s3 = startext + 3*longincr + 2*shortincr;
			}
		else {
			s3 = startext + 2*longincr + 2*shortincr;
			}
		if(shimari)
			fire_strat_rule(s3,SHIMARI_EXTEND,0,NOSQUARE,NOT_URGENT);
		if(dist < 7 && gralive[endgr] > ALIVE){
			if(gralive[endgr] <= UNSETTLED){
				fire_strat_rule(s3,EXTEND_ATTACK_UNSETTLED,toenemy[dist] +
					atkval,NOSQUARE,NOT_URGENT);
				}
			else if(gralive[endgr] <= WEAK){
				fire_strat_rule(s3,EXTEND_AND_ATTACK,toenemy[dist] +
					atkval,NOSQUARE,NOT_URGENT);
				}
			}
		}
	if(shimari && s3 != NOSQUARE && edge_shimari(startext,longincr,shortincr))
		fire_strat_rule(s3,EXTEND_FROM_SHIM,0,NOSQUARE,NOT_URGENT);

	}


/* look for extensions and invasions on the 3 and 4 lines */

void extend(void){

   if(boardsize < 13)return;
   extedge(boardsize*2+2,1,boardsize);
   extedge(boardsize*3-3,boardsize,-1);
   extedge(boardsize*2+2,boardsize,1);
   extedge(boardsize*(boardsize-3)+2,1,-boardsize);
   }


int contactfight(void){
   int g,cflag,ptr;

	/* extensions for one stone groups */
   for(g = 0; g < maxgr; ++g){
      if(!grlv[g] || grsize[g] > 1 || gralive[g] == DEAD)continue;
      if(grcolor[g] != tm)continue;


      /* extend one stone two liberty group */
      if(grlibs[g] == 2){
	 twolibextend(g);
	 }

      /* extend or hane from one stone 3 liberty group */
      if(grlibs[g] == 3 )
	      threelibextend(g);

      /* pull out of atari once */
      if(grlibs[g] == 1 &&
         lnbn[list[grlbp[g]]] > 1){
	      cflag = FALSE;
	      for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
		      if(grlibs[list[ptr]] == 1)cflag = TRUE;
	      if(edge[list[grlbp[g]]] > 1 || !cflag)
		      fire_strat_rule(list[grlbp[g]],PULL_OUT_ATARI,75*lnbn[list[grlbp[g]]],NOSQUARE,NOT_URGENT);
	      else
		      fire_strat_rule(list[grlbp[g]],ATARI_NOT_TO_EDGE,0,NOSQUARE,NOT_URGENT);
	      }
      }
   return(FALSE);
   }


void threelibextend(int g){
	int sum=0,ptr,s,onenbr,urg,defval,val;
	urg = inlist(grarmy[g],&urgdefarmies) || thalive[gralive[g]&31];
	defval = def_val(grarmy[g]);
	if(grthreatened[g])defval = 0;
	s = mvs[grpieces[g]];
	if(edge[s] <= 1)return;
	onenbr = grsize[list[grnbp[g]]];
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr])
		sum += list[ptr]-s;
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		if(list[ptr] == s + sum)continue;
		if(lnbn[list[ptr]] != 3)continue;
		val = defval;
		if(eyerec[list[ptr]])val = 0;
		if(gralive[g] <= UNSETTLED)val = 0;  /* prevent double count */
		fire_strat_rule(list[ptr],EXTEND_THREE_LIBS,val,NOSQUARE,urg);
		if(onenbr){
			val = defval;
			if(eyerec[list[ptr]-sum])val = 0;
			fire_strat_rule(list[ptr]-sum,HANE_THREE_LIBS,val,NOSQUARE,urg);
			}
		}
	}

/* extend one stone group with 2 liberties */

void twolibextend(int g){
   int s1,s2,two_lib_nbr,ptr,stonesquare,diagflag,urg;

   urg = inlist(grarmy[g],&urgdefarmies);

   stonesquare = mvs[grpieces[g]];
   if(edge[stonesquare] == 0)return;
   s1 = list[grlbp[g]];
   s2 = list[links[grlbp[g]]];
   if(lnbn[s1] < 2 && lnbn[s2] < 2 && ld[s1] != NEUTRALLD && ld[s2] !=
	NEUTRALLD)return;
   two_lib_nbr = FALSE;

   diagflag = TRUE;
   if(s2-s1 == 2 || s2-s1 == 2*boardsize)diagflag = FALSE;

   for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
      if(grlibs[list[ptr]] == 2 && armysize[grarmy[list[ptr]]] == 1)
         two_lib_nbr = TRUE;

   for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
	   s1 = list[ptr];
	   if(links[ptr] == EOL)s2 = list[grlbp[g]];
	   else s2 = list[links[ptr]];

	   if(lnbn[s1] == 0)continue;
	   if(lnbn[s1] == 1){
		   if(lnbf[s1][1-grcolor[g]] == 0)continue;
		   if(lnbn[s2] <= 1 && lnbf[s2][1-grcolor[g]] == 0)
			   continue;
		   }

	   fire_strat_rule(s1,EXTEND_2_LIBS,0,NOSQUARE,urg);
	   if(edge[s1] <= 1 && edge[stonesquare] > 1){
		   fire_strat_rule(s1,NOT_TO_EDGE,0,NOSQUARE,NOT_URGENT);

		   if(grthreatened[g] && edge2[s1] > 2 && 
		      !rule_fired(s1,CUT_AT_EDGE))
			   fire_strat_rule(s1,REALLY_NOT_TO_EDGE,0,NOSQUARE,NOT_URGENT);
		   }
	   else if(two_lib_nbr)fire_strat_rule(s1,TWO_LIB_NBR,0,NOSQUARE,NOT_URGENT);
	   else if(lnbn[s1] == 3){
		   fire_strat_rule(s1,GET_3_LIBS,0,NOSQUARE,NOT_URGENT);
		   if(diagflag && ld[s2] >= 4)
			   fire_strat_rule(s1,PREVENT_CORNER_TURN,0,NOSQUARE,NOT_URGENT);
		   }
	   else if(lnbn[s1] == 2)
		   fire_strat_rule(s1,GET_2_LIBS,0,NOSQUARE,NOT_URGENT);
	   }

   }


/* value of capturing or saving cutting stones, group g
 * add up values of all cuts involving this stone.
 */

int cut_stones_val(int g){
	int cutval,ptr,ptr2,g1,g2,armylist=EOL,s,sn,i,ldtmp;
	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
		addlist(grarmy[list[ptr]],&armylist);
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(ld[s] != NEUTRALLD)continue;
		i = fdir[s];
		for(ldtmp = ldir[i]; i < ldtmp; ++i){
			sn = s + nbr[i];
			if(grcolor[board[sn]] == 1-grcolor[g])
				addlist(grarmy[board[sn]],&armylist);
			}
		}
	cutval = 0;
	for(ptr = armylist; ptr != EOL; ptr = links[ptr])
		for(ptr2 = links[ptr]; ptr2 != EOL; ptr2 = links[ptr2]){
			g1 = list[armygroups[list[ptr]]];
			g2 = list[armygroups[list[ptr2]]];
			cutval += cut_val(g1,g2);
			}
	killist(&armylist);
	return(cutval);
	}






/* determine value for saving a threatened group (g) and find all
 * of the moves that save it.
 * Value of saving group is two points per stone in group.
 *   plus one point for every liberty after the first.
 * If group is cutting stones there is additional value.
 * If neighbors of group are unsettled there is additional value.
 * Possible saving moves are capturing neighboring groups with one liberty
 * and extending in own liberties.
 */


void save_th_group(int g){
	int val,ptr,ptr2,cutval,s,sn,tmplist= EOL,tmp2list = EOL,i,ldtmp;
	int savemovelist = EOL,iseye,urg;
	val = def_val(grarmy[g]);
	urg = inlist(grarmy[g],&urgdefarmies);
	if(grsize[g] == 1 && grlibs[g] == 1 && ld[list[grlbp[g]]] == 8)
		val = 30;	/* ko capture 1/2 point */

	cutval = cut_stones_val(g);	/* see if cutting stones */
	iseye = FALSE;
	iseye = grsize[g] > 1 && eyepot[eyerec[mvs[grpieces[g]]]] != 0;

	cpylist(grnbp[g],&tmplist);
	for(ptr = tmplist; ptr != EOL; ptr = links[ptr]){
		if(grlibs[list[ptr]] <= grlibs[g]){
			cpylist(grlbp[list[ptr]],&tmp2list);
			for(ptr2 = tmp2list; ptr2 != EOL; ptr2 = links[ptr2]){
				s = list[ptr2];
				if(!canbecaptured(s,tm,mvs[grpieces[g]],1-tm)){
					addlist(s,&savemovelist);
					}
				}
			killist(&tmp2list);
			}
		}
	killist(&tmplist);


	cpylist(grlbp[g],&tmplist);
	for(ptr = tmplist; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(!canbecaptured(s,tm,mvs[grpieces[g]],1-tm)){
			addlist(s,&savemovelist);
			}
		cpylist(nblbp[s],&tmp2list);
		for(ptr2 = tmp2list; ptr2 != EOL; ptr2 = links[ptr2]){
			sn = list[ptr2];
			if(!canbecaptured(sn,tm,mvs[grpieces[g]],1-tm))
				addlist(sn,&savemovelist);
			}
		killist(&tmp2list);
		if(lnbn[s] < 2 && lnbf[s][1-tm] == 0){
			i = fdir[s];
			for(ldtmp = ldir[i]; i < ldtmp; ++i){
				if(ld[s+nbr[i]] == 0 && board[s+nbr[i]] != g){
					cpylist(grlbp[board[s+nbr[i]]],&tmp2list);
					for(ptr2 = tmp2list; ptr2 != EOL; ptr2 = links[ptr2]){
						sn = list[ptr2];
						if(!canbecaptured(sn,tm,mvs[grpieces[g]],1-tm))
							addlist(sn,&savemovelist);
						}
					killist(&tmp2list);
					}
				}
			}
		}

	for(ptr = savemovelist; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		fire_strat_rule(s,SAVE_TH_GROUP,val,NOSQUARE,urg);
		if(cutval != 0)
			fire_strat_rule(s,SAVE_CUTTING_STONES,cutval,NOSQUARE,NOT_URGENT);
		if(iseye)
			fire_strat_rule(s,SAVE_GROUP_ATTACK,0,NOSQUARE,NOT_URGENT);
		}
	killist(&tmplist);
	killist(&savemovelist);
	}	


/* put stone at s, color c and check if group with stone at gs can be captured
 * if ctm moves first.  Return TRUE if can be captured.
 */


int canbecaptured(int s,int c,int gs,int ctm){
	int can_cap;

	mvs[msptr] = s;
	mvcolor[msptr] = c;
	if(lupdate(msptr)){
		++msptr;
		if(board[gs] != NOGROUP)
	   		can_cap = iscaptured(board[gs],80,playlevel,taclibs,ctm,NOGROUP);
		else
			can_cap = TRUE;
		--msptr;
		}
	else
		can_cap = FALSE;
	ldndate(msptr);
	return(can_cap);
	}

/* try to save an UNSETTLED, WEAK, or THREATENED group */

void savegroup(void){
	int g,armylist = EOL,ptr;
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		if(grcolor[g] != tm)continue;
		if(grthreatened[g]){
			save_th_group(g);
			continue;
			}
		if(gralive[g] > WEAK)continue;
		if(gralive[g] > ALIVE){
			addlist(grarmy[g],&armylist);
			continue;
			}
		}
	for(ptr = armylist; ptr != EOL; ptr = links[ptr]){
		if(gralive[list[armygroups[list[ptr]]]] > UNSETTLED)
			save_weak_army(list[ptr]);
		else if(gralive[list[armygroups[list[ptr]]]] == SEMEAI)
			save_semeai(list[ptr]);
		else
 			save_unsettled_army(list[ptr]);
		}
	killist(&armylist);
	}


/* find moves that make eye shape for army */
/* return TRUE if found one.  Note is called from two places */

int make_eye_shape(int army,int val,int urg,int* slist){
	int ptr,ptr2,ptr3,s,sn,numpot,flag = FALSE,eslist = EOL;
	for(ptr = armyvitalpoints[army]; ptr != EOL; ptr = links[ptr]){
		if(addlist(list[ptr],slist))
			fire_strat_rule(list[ptr],SAVE_UNSETTLED_VITAL,val,NOSQUARE,urg);
		else
			fire_strat_rule(list[ptr],SAVE_UNSETTLED_VITAL,0,NOSQUARE,urg);
		flag = TRUE;
		}
	for(ptr = armylbp[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(lnbn[s] != 2 || ld[s] < 4 || ld[s] > 5)continue;
		for(ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = links[ptr2]){
			sn = list[ptr2];
		
			numpot = 0;
			for(ptr3 = nblbp[sn]; ptr3 != EOL; ptr3 = links[ptr3]){
				if(lnbn[list[ptr3]] == 2 && ld[list[ptr3]] >= 2 &&
				   ld[list[ptr3]] <= 5 &&
				   list[ptr3] != s &&
				   grcolor[lgr[list[ptr3]]] == tm)
					numpot += ld[list[ptr3]];
				}
			if(edge[sn] != 1 && lnbf[sn][1-tm] == 0 &&
			   addlist(sn,&eslist)){
				if(addlist(sn,slist))
					fire_strat_rule(sn,MAKE_EYE_SHAPE,val+20*numpot,NOSQUARE,urg);
				else
					fire_strat_rule(sn,MAKE_EYE_SHAPE,20*numpot,NOSQUARE,urg);
					
				flag = TRUE;
				}
			}
		}
	killist(&eslist);
	return(flag);
	}



/* find moves which might save an unsettled group
 * any liberty of group
 * any liberty of liberty
 * any liberty of neighboring group
 * make a partial eye
 * defend territory
 * connect to another group is handled by cut_connect
 * any vital point
 */


void save_unsettled_army(int army){
	int ptr,ptr2,s,tmplist = EOL,g,defval,c;
	int defpot,sg,best,urg,slist = EOL;
	urg = inlist(army,&urgdefarmies);
	sg = mvs[grpieces[list[armygroups[army]]]];
	c = grcolor[list[armygroups[army]]];
	defval = def_val(army);
	if(grthreatened[list[armygroups[army]]])defval = 0;
	defpot = defval/2 + 100;
	if(defpot > 300)defpot = 300;

	if(gralive[list[armygroups[army]]] == UNSETTLED_RUN)
 	    for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		tmplist = adpot(army,list[ptr]);
		for(ptr2 = tmplist; ptr2 != EOL; ptr2 = links[ptr2])
			if(addlist(list[ptr2],&slist))
				fire_strat_rule(list[ptr2],SAVE_UNSETTLED_RUN,defval,NOSQUARE,urg);
		killist(&tmplist);
		}
	else
 	    for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		tmplist = adpot(army,list[ptr]);
		for(ptr2 = tmplist; ptr2 != EOL; ptr2 = links[ptr2])
			if(addlist(list[ptr2],&slist))
				fire_strat_rule(list[ptr2],SAVE_UNSETTLED_GROUP,defval,NOSQUARE,urg);
		killist(&tmplist);
		}
	cpylist(armylbp[army],&tmplist);
	for(ptr = armylbp[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		mrglist(nblbp[s],&tmplist);
		}
	for(ptr = tmplist; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(lnbf[s][1-c] == 1 && lnbn[s] >= 2 && 
		   !grthreatened[lgr[s]] && addlist(s,&slist)){
			fire_strat_rule(s,DEFEND_EYE_SPACE,defpot,NOSQUARE,urg);
			}
		}

	for(ptr = armygroups[army]; ptr != EOL; ptr = links[ptr]){
		g = list[ptr];
		for(ptr2 = grnbp[g]; ptr2 != EOL; ptr2 = links[ptr2])
			if(gralive[list[ptr2]] > ALIVE)
				mrglist(grlbp[list[ptr2]],&tmplist);
		}

	for(ptr = tmplist; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(addlist(s,&slist))
			fire_strat_rule(s,SAVE_UNSETTLED_GROUP,defpot,sg,urg);
	        }

	make_eye_shape(army,0,urg,&slist);
	best = bestpot(army);
	if(sumpots[armyterr[army]] > best) best = sumpots[armyterr[army]];
	if(best + armyeyespace[army] < 16)runaway(army,urg,&slist);

	killist(&tmplist);
	killist(&slist);
	}

/* fill the outside liberties of a neighboring army */


void filloutside(int army,int nbrarmy,int val,int urg,int* slist){
	int ptr,s,groupv,oflag;
	groupv = 150 - 10*armylibs[nbrarmy];
	if(groupv < 0)groupv = 0;
	if(armysize[nbrarmy] < 3)groupv = 0;
	oflag = FALSE;
	for(ptr = armylbp[nbrarmy]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(!inlist(s,&armylbp[army])){
			if(addlist(s,slist))
				fire_strat_rule(s,SEMEAI_OUTSIDE_LIB,val+groupv+lnbn[s],NOSQUARE,urg);
			else
				fire_strat_rule(s,SEMEAI_OUTSIDE_LIB,groupv+lnbn[s],NOSQUARE,urg);
			oflag = TRUE;
			}
		}
	if(!oflag)
		for(ptr = armylbp[nbrarmy]; ptr != EOL; ptr = links[ptr]){
			s = list[ptr];
			if(inlist(s,&armylbp[army])){
				if(addlist(s,slist))
					fire_strat_rule(s,SEMEAI_INSIDE_LIB,val+groupv+lnbn[s],NOSQUARE,urg);
				else
					fire_strat_rule(s,SEMEAI_INSIDE_LIB,groupv+lnbn[s],NOSQUARE,urg);
				}
			}
	}


/* army is involved in a Semeai.  play to save it
 * fill outside liberty
 */

void save_semeai(int army){
	int ptr,val,s,ptr2,lcount,nbrlist = EOL,urg,slist = EOL,tmplist;

        urg = inlist(army,&urgdefarmies);
	val = def_val(army);

	for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		if(pots[list[ptr]].pot_val == 0)continue;
		tmplist = adpot(army,list[ptr]);
		for(ptr2 = tmplist; ptr2 != EOL; ptr2 = links[ptr2])
			if(addlist(list[ptr2],&slist))
				fire_strat_rule(list[ptr2],SEMEAI_EYESPACE,val,NOSQUARE,urg);
		killist(&tmplist);
		}


	if(armynbp[army] == EOL)getarmynbp(army);
	for(ptr = armynbp[army]; ptr != EOL; ptr = links[ptr]){
		if(gralive[list[armygroups[list[ptr]]]] > 12 && 
		   semeai_result(army,list[ptr]) == 0)
			addlist(list[ptr],&nbrlist);
		}



	for(ptr = nbrlist; ptr != EOL; ptr = links[ptr])
		for(ptr2 = armyvitalpoints[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2])
			if(addlist(list[ptr2],&slist))
				fire_strat_rule(list[ptr2],SEMEAI_NBR_VITAL,val,NOSQUARE,urg);

	for(ptr = armylbp[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(lnbn[s] <= 1)continue;
		lcount = 0;
		for(ptr2 = nblbp[s]; ptr2 != EOL; ptr2 = links[ptr2])
			if(ld[list[ptr2]] == NOLD || 
			   ld[list[ptr2]] != NEUTRALLD && grarmy[lgr[list[ptr2]]]
			   != army)++lcount;
			   
		if(lcount && addlist(s,&slist))
			fire_strat_rule(s,SEMEAI_MORE_LIBS,val+50*lcount,NOSQUARE,urg);
		}

	for(ptr = nbrlist; ptr != EOL; ptr = links[ptr])
		filloutside(army,list[ptr],val,urg,&slist);

	make_eye_shape(army,val,urg,&slist);

	runaway(army,urg,&slist);

	killist(&nbrlist);
	killist(&slist);
	}

// sm: bugfix: the 'rnval' array is indexed up to NUMRUN-1
// in the loop below (program arguments "50 9"), and NUMRUN is 8,
// so I've added two more '0' initializers
int rnval[] = { 150,100,100,0,0,0 ,0,0 };


void runaway(int army,int urg,int* rlist){
	int ptr,val,s,i;
	if(armyrn_pot[army] >= 16)return;
	if(armylibs[army] == 2)return;
	if(armyrn_pot[army] >= EASY_RUN)val = 0;
	else if(grthreatened[list[armygroups[army]]])val = 0;
	else
		val = def_val(army);

	for(i = 0; i < NUMRUN; ++i)
		for(ptr = armyrun[army][i]; ptr != EOL; ptr = links[ptr]){
			s = list[ptr];
			runhere(army,val+rnval[i],s,rlist,urg,i);
			}
	killist(rlist);
	}

void runhere(int army,int val,int s,int* rlist,int urg,int type){
	int ptr,i,sn,ldtmp,eyflag,numopen,ptr2,c;
	if(type == 6 && ld[s] == NEUTRALLD || type == 7){
		if(addlist(s,rlist)){
			fire_strat_rule(s,RUN_AWAY,val,NOSQUARE,urg);
			}
		return;
		}
	c = grcolor[list[armygroups[army]]];
	if(lnbn[s] == 3){
	   for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
		   if(lnbn[list[ptr]] == 4 && (edge[list[ptr]] > 3 || edge[list[ptr]] > edge[s])){
			   eyflag = FALSE;
			   numopen = 0;
			   for(ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
				   if(lnbn[list[ptr2]] == 4)numopen++;
				   if(ld[list[ptr2]] >= 4 && ld[list[ptr2]] < 9)
					   eyflag = TRUE;
				   }
			   val += 50*numopen;
			   if(numopen > 0 && !eyflag && addlist(list[ptr],rlist)){
				   if(rterv[list[ptr]][c] > rterv[s][c])
					   fire_strat_rule(list[ptr],TOWARD_STRENGTH,0,NOSQUARE,NOT_URGENT);
				   fire_strat_rule(list[ptr],RUN_AWAY,val,NOSQUARE,urg);
				   }
			   if(board[s+s-list[ptr]] != NOGROUP && numopen == 3 &&
			      armysize[army] < 3)
				   for(ptr2 = nblbp[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
					   if(lnbn[list[ptr2]] != 4)continue;
					   if(!addlist(list[ptr2],rlist))continue;
					   if(rterv[list[ptr2]][c] > rterv[s][c])
						   fire_strat_rule(list[ptr2],TOWARD_STRENGTH,0,NOSQUARE,NOT_URGENT);
					   fire_strat_rule(list[ptr2],RUN_AWAY,val,NOSQUARE,urg);
					   }
			   }
		   }
	   }
	if(ld[s] >= 4){
		i = fdir[s];
		for(ldtmp = ldir[i]; i < ldtmp; ++i){
			sn = s + nbr[i];
			if(grarmy[board[sn]] == army && lnbn[s-nbr[i]] == 4 &&
			   lnbn[s-nbr[i]-nbr[i]] == 4 && addlist(s-nbr[i]-nbr[i],rlist))
			   fire_strat_rule(s-nbr[i]-nbr[i],RUN_AWAY,val+150,NOSQUARE,urg);
			}
		}
	}

/* find moves to look at which might save a weak group
 */

void save_weak_army(int army){
	int ptr,ptr2,tmplist = EOL,def,urg,t2list=EOL,pval;

	def = def_val(army);
	if(grthreatened[list[armygroups[army]]])def = 0;

	urg = inlist(army,&urgdefarmies);

	for(ptr = armyvitalpoints[army]; ptr != EOL; ptr = links[ptr])
		fire_strat_rule(list[ptr],SAVE_VITAL,0,NOSQUARE,urg);


	for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		tmplist = adpot(army,list[ptr]);
		pval = pots[list[ptr]].pot_val;
		if(pots[list[ptr]].pot_type == UNDERCUT)
			pval = sumeyes[armyterr[army]+pval]-sumeyes[armyterr[army]];
		for(ptr2 = tmplist; ptr2 != EOL; ptr2 = links[ptr2])
			if(addlist(list[ptr2],&t2list))
			   fire_strat_rule(list[ptr2],SAVE_WEAK_GROUP,def+pval*10,NOSQUARE,urg);
		killist(&tmplist);
		}
	runaway(army,urg,&t2list);	/* try to run away */

	make_eye_shape(army,0,urg,&t2list);

	killist(&t2list);
	}



void remove_dead_stones(void){
        int g,ptr;
        for(g = 0; g < maxgr; ++g){
                if(!grlv[g] || grcolor[g] == tm || gralive[g] <= ALIVE)
                          continue;
                for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
                         fire_strat_rule(list[ptr],REMOVE_DEAD,0,NOSQUARE,NOT_URGENT);
                         if(grlibs[g] > 1 && lnbn[list[ptr]] == 1 &&
                                 lnbf[list[ptr]][1-grcolor[g]] == 0)
                                 fire_strat_rule(list[nblbp[list[ptr]]],NOT_URGENT,
                                      REMOVE_DEAD,0,NOSQUARE);
                         }
                }
        }


/* attack weak enemy groups and kill threatened enemy groups
 * attacking by saving a nbr group is handled elsewhere
 */


void killgroup(void){
	int g,armylist = EOL,ptr,ptr2,army,alive,s;
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		if(grcolor[g] == tm)continue;
		if(gralive[g] == DEAD)continue;
		if(grthreatened[g]){
			kill_th_group(g);
			continue;
			}
		if(grlibs[g] == 2)
			for(ptr2 = grlbp[g]; ptr2 != EOL; ptr2 = links[ptr2]){
				s = list[ptr2];
				if(gralive[lgr[s]] == DEAD && grcolor[lgr[s]] == tm)
					continue; /* don't atari from dead group */
				if(lnbn[s] > 1)
					fire_strat_rule(s,TRY_ATARI,0,NOSQUARE,NOT_URGENT);
		/*		else if(lnbn[s] == 1 && lnbn[list[nblbp[s]]] < 4)
					fire_strat_rule(s,TRY_ATARI,0,NOSQUARE); ,NOT_URGENT*/
				else if(ld[s] == NEUTRALLD)
					fire_strat_rule(s,TRY_ATARI,0,NOSQUARE,NOT_URGENT);
				}
		if(gralive[g] <= STRONG_MIAI)continue;
		if(gralive[g] > WEAK)continue;
		addlist(grarmy[g],&armylist);
		}
	for(ptr = armylist; ptr != EOL; ptr = links[ptr]){
		army = list[ptr];
		alive = gralive[list[armygroups[army]]];
		if(alive > ALIVE)poke_eye(army);
		else if(kosquare != NOSQUARE)
			poke_eye_ko(army);
		attack_weak_army(army);
		}
	killist(&armylist);
   	}

/* find move to attack weak, unsettled, or miaialive army
 * blocking extensions are handled elsewhere
 */

void attack_weak_army(int army){
	int s,ptr,ptr2,sn,g,i,numr=0,r1=NOSQUARE,r2=NOSQUARE,slist = EOL,aval,rlist,val;
	aval = atk_val(army);
	for(ptr = armypot[army]; ptr != EOL; ptr = links[ptr]){
		rlist = rmpot(army,list[ptr]);
		for(ptr2 = rlist; ptr2 != EOL; ptr2 = links[ptr2])
			if(addlist(list[ptr2],&slist)){
			   val = 0;
			   if(pots[list[ptr]].pot_val >= 8 ||
			      pots[list[ptr]].pot_type == UNDERCUT &&
			      pots[list[ptr]].pot_val >= 2)val = aval;
			   else if(pots[list[ptr]].pot_val != 0)
				   val = aval/2;
			   if(pots[list[ptr]].pot_type == EXTEND){
				   if(addlist(list[ptr2],&slist))
				      fire_strat_rule(list[ptr2],ATTACK_SURROUND,
						   val,NOSQUARE,NOT_URGENT);
				   }
			   else if(pots[list[ptr]].pot_type == CONNECT)
				   fire_strat_rule(list[ptr2],ATTACK_CUT,
						   val,NOSQUARE,NOT_URGENT);
			   else if(pots[list[ptr]].pot_type != THREAT)
				   fire_strat_rule(list[ptr2],ATTACK_EYE_POT,val,NOSQUARE,NOT_URGENT);
			   }
		killist(&rlist);
		}
	if(armyrn_pot[army] == 0){
		killist(&slist);
		return;
		}
	g = list[armygroups[army]];
	s = mvs[grpieces[g]];
	if(armysize[army] == 1)
		for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
			sn = list[ptr];
			if(edge[sn] > edge[s] && ld[2*sn-s] == NOLD &&
			   addlist(2*sn-s,&slist)){
				fire_strat_rule(2*sn-s,ATTACK_WITH_CAP,aval,NOSQUARE,NOT_URGENT);
				}
			}
	for(i = 0; i < NUMRUN; ++i){
		for(ptr = armyrun[army][i]; ptr != EOL; ptr = links[ptr]){
			++numr;
			if(r1 == NOSQUARE)r1 = list[ptr];
			else if(r2 == NOSQUARE)r2 = list[ptr];
			surround(army,list[ptr],&slist,aval);
			}
		if(numr > 3)break;
		}
	if(numr == 2 && 
	   (abs(r1-r2) == boardsize-1 || abs(r1-r2) == boardsize+1) &&
	   lnbn[r1] == 3 && lnbn[r2] == 3){
		s = xval[r1]+boardsize*yval[r2];
		sn = xval[r2]+boardsize*yval[r1];
		if(board[s] == NOGROUP && (edge[s] > edge[sn] || edge[s] > 4) &&
		   addlist(s,&slist))
		   fire_strat_rule(s,ATTACK_SURROUND,aval,NOSQUARE,NOT_URGENT);
		if(board[sn] == NOGROUP && (edge[sn] > edge[s] || edge[sn] > 4) &&
		   addlist(sn,&slist))
		   fire_strat_rule(sn,ATTACK_SURROUND,aval,NOSQUARE,NOT_URGENT);
		}
	killist(&slist);
	}


/* s is a running point for army.  Surround army by filling running point */

void surround(int army,int s,int* slist,int aval){
	int ptr,c,sn,bval,sval,ctflag,i,ldtmp;
	c = grcolor[list[armygroups[army]]];
	if(ld[s] == NEUTRALLD){
		if(addlist(s,slist))
			fire_strat_rule(s,ATTACK_SURROUND,aval,NOSQUARE,NOT_URGENT);
		if(lnbn[s] == 2){
			i = fdir[s];
			for(ldtmp = ldir[i]; i < ldtmp; ++i){
				if(grcolor[board[s+nbr[i]]] == 1-c && 
				   addlist(s-nbr[i],slist))
					fire_strat_rule(s-nbr[i],ATTACK_SURROUND,aval,NOSQUARE,NOT_URGENT);
				
				}
			}
		}
	else if(ld[s] == 4 && lnbn[s] == 2){
		for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr])
			if(lnbf[list[ptr]][c] == 0 && 
			   edge[list[ptr]] >= edge[s] &&
			   lnbf[list[ptr]+list[ptr]-s][1-c] == 0 &&
			   grcolor[board[list[ptr]+list[ptr]-s]] != 1-c &&
			   addlist(list[ptr],slist))
			   fire_strat_rule(list[ptr],ATTACK_SURROUND,aval,NOSQUARE,NOT_URGENT);
		}
	else if(lnbn[s] == 3){
		bval = 1000;
		sval = NOSQUARE;
		ctflag = FALSE;
		for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
			sn = list[ptr];
			if(lnbf[sn][1-c] != 0 && lnbf[sn][c] == 0)
				ctflag = TRUE;
			if(lnbf[sn][c] == 0 && rterv[sn][1-c] > 0 && 
			   (edge[sn] > edge[s] || edge[sn] > 3) && 
			   addlist(sn,slist)){
				if(rterv[sn][1-c] < bval){
					bval = rterv[sn][1-c];
					sval = sn;
					}
				}
			}
		if(sval != NOSQUARE && addlist(sval,slist))
			fire_strat_rule(sval,ATTACK_SURROUND,aval,NOSQUARE,NOT_URGENT);
		if(ctflag && addlist(s,slist))
			fire_strat_rule(s,ATTACK_SURROUND,aval,NOSQUARE,NOT_URGENT);
		}
	}


/* play in key spot to kill eye */

void poke_eye(int army){
	int ptr,s;
	for(ptr = armyvitalpoints[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		fire_strat_rule(s,POKE_EYE,0,NOSQUARE,NOT_URGENT);
		}
	}

/* play in key spot to kill eye */

void poke_eye_ko(int army){
	int ptr,s,atkval;
	atkval = atk_val(army);
	for(ptr = armyvitalpoints[army]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		fire_strat_rule(s,POKE_EYE_KO,atkval+100,NOSQUARE,NOT_URGENT);
		}
	}



/* find potential geta moves for caturing group g */

int getamoves(int g){
	int tmplist = EOL;
	int ptr,s,s2;
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		if(links[ptr] == EOL)break;
		s = list[ptr];
		s2 = list[links[ptr]];
		if(s2-s == boardsize+1){
			if(board[s2-1] == NOGROUP)addlist(s2-1,&tmplist);
			if(board[s+1] == NOGROUP)addlist(s+1,&tmplist);
			}
		if(s2-s == boardsize-1){
			if(board[s2+1] == NOGROUP)addlist(s2+1,&tmplist);
			if(board[s-1] == NOGROUP)addlist(s-1,&tmplist);
			}
		}
	return(tmplist);
	}


/* find approach moves to try to capture g */

int approachmoves(int g){
	int tmplist = EOL,ptr,s,i,sn,ptr2,ldtmp;
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(lnbn[s] > 1)continue;
		if(lnbn[s] == 1)addlist(list[nblbp[s]],&tmplist);
		if(lnbn[s] == 0 && lnbf[s][1-grcolor[g]] == 1){
			i = fdir[s];
			for(ldtmp = ldir[i]; i < ldtmp; ++i){
				sn = s + nbr[i];
				if(grcolor[board[sn]] == 1-grcolor[g] &&
				   grlibs[board[sn]] == 2)
					for(ptr2 = grlbp[board[sn]]; ptr2 != EOL; ptr2 = links[ptr2]){
						if(list[ptr2] != s)
							addlist(list[ptr2],&tmplist);
						}
				}
			}
		}
	return(tmplist);
	}

void kill_th_group(int g){
	int ptr,tmplist = EOL,s,sn;
        int defval;  /* value of defending neighboring groups */
        int cutval;  /* value of capturing cutting stones */
	int t2,urg;

	urg = FALSE;
	if(armynbp[grarmy[g]] == EOL)getarmynbp(grarmy[g]);
	for(ptr = armynbp[grarmy[g]]; ptr != EOL; ptr = links[ptr]){
		if(inlist(list[ptr],&urgdefarmies)){
			urg = TRUE;
			break;
			}
		}

	defval = 0;
	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
		if(gralive[list[ptr]] > ALIVE)defval++;


	cutval = cut_stones_val(g);	/* see if cutting stones */

        if(!cutval && !defval && gralive[g] > WEAK)return;
        
        /* Don't bother capturing extremely weak groups */ 
      
	if(grlibs[g] == 1 && (defval || cutval))
		fire_strat_rule(list[grlbp[g]],TAKE_OFF_BOARD,0,NOSQUARE,NOT_URGENT);

	/* find moves to try to capture this group */
	cpylist(grlbp[g],&tmplist);
	t2 = getamoves(g);
	mrglist(t2,&tmplist);
	killist(&t2);
	t2 = approachmoves(g);
	mrglist(t2,&tmplist);
	killist(&t2);
	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
		if(grthreatened[list[ptr]])
			mrglist(grlbp[list[ptr]],&tmplist);
		}

	for(ptr = tmplist; ptr != EOL; ptr = links[ptr]){
		s = list[ptr];
		if(canbecaptured(s,tm,mvs[grpieces[g]],1-tm)){
			fire_strat_rule(s,CAPTURE_THREATENED,0,NOSQUARE,urg);
			if(ahead > 2)
				fire_strat_rule(s,CAPTURE_SAFE,0,NOSQUARE,NOT_URGENT);
			if(cutval != 0)
				fire_strat_rule(s,CAPTURE_CUTTING_STONES,cutval,NOSQUARE,NOT_URGENT);
			if(defval != 0)
				fire_strat_rule(s,CAPTURE_DEF_WEAK,defval,NOSQUARE,NOT_URGENT);
			}
		else if(lnbn[s] == 1){
			sn = list[nblbp[s]];
			if(canbecaptured(sn,tm,mvs[grpieces[g]],1-tm)){
				fire_strat_rule(sn,CAPTURE_THREATENED,0,NOSQUARE,urg);
				if(cutval != 0)
					fire_strat_rule(sn,CAPTURE_CUTTING_STONES,cutval,NOSQUARE,NOT_URGENT);
				if(defval != 0)
					fire_strat_rule(sn,CAPTURE_DEF_WEAK,defval,NOSQUARE,NOT_URGENT);
				}
			}
		}
	killist(&tmplist);
	}


void kothreat(void){
   int g,ptr,s;
   for(g = 0; g < maxgr; ++g){
      if(!grlv[g])continue;
      if(grcolor[g] == tm)continue;
      if(grlibs[g] != 2 || gralive[g] > ALIVE)continue;
      if(list[grlbp[g]] == kosquare || list[links[grlbp[g]]] == kosquare)
         continue;
      if(badkothreat(g))continue;
      for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
	    s = list[ptr];
	    if(lnbn[s] != 0 || ld[s] == NEUTRALLD)
		    fire_strat_rule(list[ptr],ATARI_KOTHREAT,grsize[g] * 50,NOSQUARE,NOT_URGENT);
            }
      }
   }

/* return TRUE if ko threat to capture g is a bad one */

int badkothreat(int g){
	int i,ldtmp,c,s,ptr;
	c = grcolor[g];
	i = fdir[kosquare];
	for(ldtmp = ldir[i]; i < ldtmp; ++i){
		s = kosquare + nbr[i];
		if(grcolor[board[s]] == c && grlibs[board[s]] == 1 &&
		   grsize[board[s]] == 1)break;  /* found ko stone */
		}
	for(ptr = grnbp[board[s]]; ptr != EOL; ptr = links[ptr]){
		if(grlibs[list[ptr]] != 1)continue;
		/* found neighbor in atari */
		if(inlist(g,&grnbp[list[ptr]]))return(TRUE);
		}
	return(FALSE);
	}

int connect_bamboo(int g1,int g2,int s){
	int i,ldtmp,sn,sp,ptr;
	if(edge[s] > 2){
		for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr]){
			sp = list[ptr];
			i = fdir[sp];
			for(ldtmp = ldir[i]; i < ldtmp; ++i){
				sn = sp + nbr[i];
				if((board[sn] == g1 || board[sn] == g2) &&
				   board[sp-nbr[i]] == NOGROUP && sp-nbr[i] != s){
					return(sp-nbr[i]);
					}
				}
			}
		}
	return(NOSQUARE);
	}

/* connect two groups together */

void try_connect(int cn,int g1,int g2){
	int val,s,urg;
	if(cnprot[cn] == SOLID_CONNECT)return;
	urg = inlist(grarmy[g1],&urgdefarmies) || inlist(grarmy[g2],&urgdefarmies);
	val = conn_val(g1,g2);
	if(grthreatened[g1] || grthreatened[g2])val = 25;
              /* let saving threatened stones determine value */
	if(cnprot[cn] == AJI_CONNECT && val > 100)val = 100;
	if(cncnum[cn] == 1){  /* hane connections handled by shapes */
		s = list[cnptr[cn]];
		if(cntype[cn] == CN_ONEPOINTJUMP){
			fire_strat_rule(s,SINGLE_CONNECTION,val,NOSQUARE,urg);
			s = connect_bamboo(g1,g2,s);
			if(s != NOSQUARE)
				fire_strat_rule(s,CONNECT_BAMBOO,val,NOSQUARE,urg);
			}
		}
	else if(cnlknum[cn] == 1){
		if(canconnlink(cn,&s))
			fire_strat_rule(s,SINGLE_LINK,val,NOSQUARE,urg);
		}
	else if(cnlknum[cn] == 2)
		connect_2_links(cn,val,urg);
	}

void connect_2_links(int cn,int val,int urg){
	int s;
	if(cntype[cn] == CN_KNIGHTSMOVE && canconnlink(cn,&s))
		fire_strat_rule(s,CONNECT_KNIGHTS,val,NOSQUARE,urg);
    /* only doing one side of move since I'm in a hurry */
	}


/* strategy for cutting and connecting */
/* only connecting long linkages */


int cut_connect(void){
   int cn,c,g1,g2;
   for(cn = 0; cn < NUMCONNS; ++cn){
      if(cncnum[cn] == 0 && cnlknum[cn] == 0)continue;
      g1 = cngr1[cn];
      g2 = cngr2[cn];
      if(grarmy[g1] == grarmy[g2])continue;
      if(gralive[g1] == DEAD || gralive[g2] == DEAD)continue;
      /* found cutting point */
      c = grcolor[g1];
      if(c == tm){
	      try_connect(cn,g1,g2);
	      continue;
	      }
      else if(cncnum[cn] == 1){	/* direct cut */
	 direct_cut(cn,g1,g2);
         }
      else if(cnlknum[cn] == 1){ /* one link */
         onelink_cut(cn,g1,g2);
         }
      else if(cntype[cn] == CN_KNIGHTSMOVE){
/* cutting knights moves is handled by shapes */
         }
      }
   return(FALSE);
   }




/* return true if can connect single lkg (large knights move) at cn */
/* connsqr has point to play to connect. othsqr is other linkage point */

int canconnlkg(int cn,int* connsqr,int* othsqr){
	int c,s,s1,s2,i,dir,offs,ldtmp;
	int g1;  /* group next to connection point */
	int g2;    /* other group */
	s = list[cnllptr[cn]];
	if(inlist(s,&grlbp[cngr1[cn]])){
		g1 = cngr1[cn];
		g2 = cngr2[cn];
		}
	else{
		g1 = cngr2[cn];
		g2 = cngr1[cn];
		}
	c = grcolor[g1];
	i = fdir[s];
	for(ldtmp = ldir[i]; i < ldtmp; ++i){
		offs = nbr[i];
		dir = dirnm[i];
		if(dstbrd[s][dir] == 2 && board[sqrbrd[s][dir]] == g2)break;
		}

	s1 = s + offs;
	s2 = s1+offs;
	if(ld[s] >=4 && ld[s2] >= 2 && lnbf[s1][1-c] <= 1){
		*connsqr = s1;
		*othsqr = s2;
		return(TRUE);
		}
	if(ld[s] != NEUTRALLD && ld[s2] != NEUTRALLD && lnbf[s1][1-c] == 0){
		*connsqr = s1;
		*othsqr = s2;
		return(TRUE);
		}
	if(ld[s] >= 4 && lnbf[s1][1-c] == 0 && lnbn[s1] < 4){
		*connsqr = s2;
		*othsqr = s2;
		return(TRUE);
		}
		
	*connsqr = s1;
	*othsqr = s2;
	return(FALSE);
	}


/* return connection value of single linkage at cn.
 * connsqr has the point where the connection can be made
 *
 *  O @ +  or  O + @
 *  + + O      + + O
 *
 *  (libg)
 *  stonesqr othersquare
 *  lib      connsqr     (conng)
 */

int canconnlink(int cn,int* connsqr){
   int lib;  /* liberty counted as linkage */
   int i,j,ldtmp,ldtm2,sn,sn2;
   int stonesquare;  /* point next to lib with stone on it */
   int g1,g2;
   int othersquare,libg,conng,oppsqr,oppsqr2,thr;
   
   g1 = cngr1[cn];
   g2 = cngr2[cn];
   if(gralive[g1] == DEAD || gralive[g2] == DEAD)return(CANT_CONNECT);
   thr = grthreatened[g1] || grthreatened[g2];
   lib = list[cnlkptr[cn]];
   stonesquare = lib;
   *connsqr = 0;  /* for c optimizer bug */
   i = fdir[lib];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      sn = lib + nbr[i];
      if(board[sn] == g1 || board[sn] == g2){
		stonesquare = sn;
		libg = board[sn];
		break;
		}
      }
   if(libg == g1)conng = g2;
   else conng = g1;
   i = fdir[lib];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      sn = lib + nbr[i];
      if(board[sn] == NOGROUP){
         j = fdir[sn];
         for(ldtm2 = ldir[j]; j < ldtm2; ++j){
            sn2 = sn + nbr[j];
            if(board[sn2] == conng){
		*connsqr = sn;
		break;
		}
            }
         }
      }
   othersquare = *connsqr + stonesquare - lib;

   if(edge[*connsqr] == 1 && ld[*connsqr] != NEUTRALLD && grlibs[libg] > 2 && grlibs[conng] > 2 && !thr)return(AJI_CONNECT);
   if(grlibs[g1] > 2 && grlibs[g2] > 2 && !thr && gralive[lgr[othersquare]] == DEAD)return(AJI_CONNECT);
   if(edge[*connsqr] == 1 && ld[lib] != NEUTRALLD && grlibs[libg] > 1 && grlibs[conng] > 1)return(CAN_CONNECT);
   if(edge[*connsqr] == 2 && ld[*connsqr] != NEUTRALLD && grlibs[libg] > 3 &&
      grlibs[conng] > 3 && !thr)return(AJI_CONNECT);
   if(board[othersquare] == NOGROUP)return(CAN_CONNECT);  /* can make double connection */
   if(grlibs[board[othersquare]] <= 2)return(CAN_CONNECT);
   if(ld[*connsqr] != NEUTRALLD && lnbn[*connsqr] < 3)return(CAN_CONNECT);  /* can make hanging connection */
   if(grlibs[conng] < 3 || grlibs[libg] < 3)return(CANT_CONNECT);  /* cut would be atari */
   if(ld[lib] == NEUTRALLD)return(CANT_CONNECT);  /* opponent can cut */
   if(edge[stonesquare] < 2)return(CANT_CONNECT);
   oppsqr = lib*2-*connsqr;
   if(ld[oppsqr] >= 4 && grcolor[lgr[oppsqr]] != grcolor[conng])return(CANT_CONNECT);  /* opponent can cut */
   oppsqr2 = lib*2-stonesquare;
   if(ld[oppsqr2] >= 4 && grcolor[lgr[oppsqr2]] == 1-grcolor[conng])return(CANT_CONNECT);  /* opponent can cut */
   if(edge[lib] == 2 && edge[stonesquare] > edge[lib])
	   return(CAN_CONNECT);  /* can make protected connection on two line */
   if(ld[oppsqr] >= 4 || ld[oppsqr2] >= 4)return(CAN_CONNECT);  /* can quickly capture cutting stone */
   return(CANT_CONNECT);
   }


void onelink_cut(int cn,int g1,int g2){
   int connsqr,cutvalue,can_connect,can_cut;
   can_connect = canconnlink(cn,&connsqr);
   can_cut = ld[connsqr] == NEUTRALLD || lnbn[connsqr] == 3;
   if(can_cut && can_connect){
      cutvalue = cut_val(g1,g2);
      if(lnbn[connsqr] == 3)cutvalue /= 2;
      fire_strat_rule(connsqr,CAN_CONNECT_LINK,0,NOSQUARE,NOT_URGENT);
      fire_strat_rule(connsqr,CUT_LINKAGE,cutvalue,NOSQUARE,NOT_URGENT);
      }
   }



/* value of connecting groups g1 and g2
 */

int connalive[] = { 0,0,50,100,100 };
int connalive2[] = { 0,0,50,100,150 };

int conn_val(int g1,int g2){
	int numl1,numl2,cval;
	if(grarmy[g1] == grarmy[g2])return(0);
	if(gralive[g1] == DEAD || gralive[g2] == DEAD)return(0);
	if(grthreatened[g1] && grthreatened[g2])return(0);
	if(gralive[g1] > WEAK && gralive[g2] > WEAK && ahead > 2)return(0);

	numl1 = armylibs[grarmy[g1]] + armysize[grarmy[g1]] - 1;
	numl2 = armylibs[grarmy[g2]] + armysize[grarmy[g2]] - 1;
	if(gralive[g1] <= VERY_ALIVE && gralive[g2] <= VERY_ALIVE)
		return(connalive[ahead]);
	cval = 100;
	if(gralive[g1] < ALIVE && gralive[g2] < ALIVE)
		return(connalive2[ahead]);
	if(gralive[g1] <= ALIVE || gralive[g2] <= ALIVE){
		cval = connval[gralive[g1]&31] * numl1 +
			connval[gralive[g2]&31] * numl2;
	        if(!grthreatened[g1] && !grthreatened[g2])
	                 cval += connalive2[ahead];
		     /* save weak or unsettled group */
		if(cval > 300)cval = 400;
		return(cval);
		}
	if(gralive[g1] > WEAK && gralive[g2] > WEAK && ahead < 2)return(100);
	if(gralive[g1] <= WEAK || gralive[g2] <= WEAK){
		cval = numl1 * connval[gralive[g1]&31] + numl2 * connval[gralive[g2]&31];
		cval /= 2;
		if(cval > 250)cval = 250;
		if(gralive[g1] == SEMEAI)cval += grlibs[g2]*25;
		if(gralive[g2] == SEMEAI)cval += grlibs[g1]*25;	
		return(cval);
		}
	return(cval);
	}





/* value of cutting apart these two groups 
 * if either group is alive use the ctval table to 
 * figure out how much it is worth to connect to it
 * if both groups are unsettled or have some eye potential
 * also use this table
 */

int cut_val(int g1,int g2){
	int cutvalue,numl1,numl2;

	if(gralive[g1] == DEAD || gralive[g2] == DEAD)return(0);
	numl1 = armylibs[grarmy[g1]] + armysize[grarmy[g1]] - 1;
	numl2 = armylibs[grarmy[g2]] + armysize[grarmy[g2]] - 1;
	if(grthreatened[g1] && !grthreatened[g2] || 
	   grthreatened[g2] && !grthreatened[g1]){
		cutvalue = 0;
		if(gralive[g1] > WEAK && gralive[g2] <= UNSETTLED)
			cutvalue = numl1 * ctval[gralive[g1]&31];
		if(gralive[g2] > WEAK && gralive[g1] <= UNSETTLED)
			cutvalue = numl2 * ctval[gralive[g2]&31];
		return(cutvalue);
		}
	         /* can capture a group so that is better than cut */

	if(gralive[g1] <= ALIVE && gralive[g2] > ALIVE)
		cutvalue = ctval[gralive[g2]&31] * numl2;

	else if(gralive[g1] > ALIVE && gralive[g2] <= ALIVE)
		cutvalue = ctval[gralive[g1]&31] * numl1;

	else if(gralive[g1] <= ALIVE && gralive[g2] <= ALIVE){
		cutvalue = ctval[gralive[g1]&31] * numl1;
		if(ctval[gralive[g2]&31] * numl2 < cutvalue)
			cutvalue = ctval[gralive[g2]&31] * numl2;
		if(ahead < 2)cutvalue *= 2;
		if(cutvalue > 200)cutvalue = 200;
		}
	else if(gralive[g1] <= UNSETTLED && gralive[g2] <= UNSETTLED){
		cutvalue = ctval[gralive[g1]&31]*numl1;
		if(armyeyepotential[grarmy[g1]] + armyeyepotential[grarmy[g2]] + armyeyespace[grarmy[g1]] + armyeyespace[grarmy[g2]] >= 32)
			cutvalue += ctval[gralive[g2]&31] * numl2;
		else if(ctval[gralive[g2]&31] * numl2 < cutvalue)
			cutvalue = ctval[gralive[g2]&31] * numl2;
		if(cutvalue > 500)cutvalue = 500;
		}
	else if(gralive[g1] <= WEAK && gralive[g2] <= WEAK){
		cutvalue = ctval[gralive[g1]&31]*numl1;
		if(ctval[gralive[g2]&31] * numl2 < cutvalue)
			cutvalue = ctval[gralive[g2]&31] * numl2;
		if(ahead > 2)cutvalue += 150;
		if(cutvalue > 300)cutvalue = 300;
		}
	else if(gralive[g1] > WEAK && gralive[g2] > WEAK){
		if(ahead > 2)cutvalue = 50*(numl1+numl2);
		else cutvalue = 0;
		if(cutvalue > 300)cutvalue = 300;
		}
	else if(gralive[g1] > WEAK){
		cutvalue = ctval[gralive[g1]&31] * numl1;
		if(ahead > 2)cutvalue += 100;
		}
	else if(gralive[g2] > WEAK){
		cutvalue = ctval[gralive[g2]&31] * numl2;
		if(ahead > 2)cutvalue += 100;
		}

	return(cutvalue);
	}


/* value of defending army */

int def_val(int army){
	int size,val,ptr,gral,aval;
	gral = gralive[list[armygroups[army]]];
	size = armysize[army];
	if(gral != 16 && size <= 2 && links[armygroups[army]] == EOL)
		for(ptr = grlbp[list[armygroups[army]]]; ptr != EOL; ptr = links[ptr])
			if(ld[list[ptr]] != NEUTRALLD &&
			   lnbn[list[ptr]] > 1)size += lnbn[list[ptr]]-1;
	size += armylibs[army];
	if(size > 15 && gral != SEMEAI)size = 15;
	val = defv[gral];
	if(ahead >= 3 && (gral == 6 || gral == 9 || gral == 12 || gral == 13))
		val += 5;
	val *= size;
	if(gral == 6 || gral >= 9){
		if(armynbp[army] == EOL)getarmynbp(army);
		aval = 0;
		for(ptr = armynbp[army]; ptr != EOL; ptr = links[ptr])
			aval += atk_val(list[ptr]);
		if(gral == 6 || gral == 9 || gral == 12 || gral == 15){
			if(val > 200)val = 200;
			aval /= 2;
			if(aval > 200)aval = 200;
			}
		if(armysize[army] == 1 && aval > 100)aval = 100;
		val += aval;
		}
	if(val > 1000)val = 1000;
	return(val);
	}

/* value of attacking army */

int atk_val(int army){
	int size,val,gral;;
	size = armysize[army];
	size += armylibs[army];
	if(size > 15)size = 15;
	gral = gralive[list[armygroups[army]]];
	if(gral == 12 && size > 6)size = 6;
	val = atkv[gral] * size;
	if(val > 500)val = 500;
	if(armyrn_pot[army] >= EASY_RUN && val > 200)val = 200;
	return(val);
	}

/* try to cut a single connection, hane or one point jump */

void direct_cut(int cn,int g1,int g2){
   int cutpoint,controlled_nbrs,can_cut,tmplist;
   int i,ldtmp,sn,ptr,cutvalue,cut_semeai,semeaiarmy,ctnbval=0;
   cutpoint = list[cnptr[cn]];
   controlled_nbrs = 0;
   cut_semeai = FALSE;
   i = fdir[cutpoint];
   for(ldtmp = ldir[i]; i < ldtmp; ++i){
      sn = cutpoint + nbr[i];
      if(ld[sn] == 0 && grcolor[board[sn]] == tm ||
         lnbn[sn] <= 2 && ld[sn] != NEUTRALLD && grcolor[lgr[sn]] == tm){
         ++controlled_nbrs;
	 }
      if(ld[sn] == 0 && grcolor[board[sn]] == tm && gralive[board[sn]] == SEMEAI){
	 cut_semeai = TRUE;
	 semeaiarmy = grarmy[board[sn]];
	 }
      }

   if(cut_semeai){
	   if(gralive[g1] <= ALIVE && gralive[g2] <= ALIVE)cut_semeai = FALSE;
	   if(armyrn_pot[grarmy[g1]] == 0 && armyrn_pot[grarmy[g2]] == 0)
		   cut_semeai = FALSE;
	   }
      if(cut_semeai)
	      fire_strat_rule(cutpoint,CUT_WIN_SEMEAI,def_val(semeaiarmy),NOSQUARE,NOT_URGENT);

   can_cut = !canbecaptured(cutpoint,tm,cutpoint,1-tm);
   if(can_cut){	/* look for other groups which can be captured */
      tmplist = EOL;
      if(andlist(grnbp[g1],grnbp[g2],&tmplist)){
         for(ptr = tmplist; ptr != EOL; ptr = links[ptr]){
	    if(grthreatened[list[ptr]]){
		    /* unfinished code here... */
		break;
               }
            }
         killist(&tmplist);
         }
      }
   cutvalue = cut_val(g1,g2);

   if(cutvalue >= 100 && controlled_nbrs)ctnbval = 100;
   if(can_cut)fire_strat_rule(cutpoint,CAN_CUT,cutvalue,NOSQUARE,NOT_URGENT);
   if(can_cut && grlibs[g1] == 2 && grlibs[g2] == 2)
	fire_strat_rule(cutpoint,DOUBLE_ATARI,0,NOSQUARE,NOT_URGENT);
   else if(can_cut && (grlibs[g1] == 2 && grthreatened[g2] || grlibs[g2] == 2 &&
		  grthreatened[g1]))
	fire_strat_rule(cutpoint,DOUBLE_THREAT,0,NOSQUARE,NOT_URGENT);

   if(can_cut && cntype[cn] == CN_ONEPOINTJUMP && controlled_nbrs){
/* can push through */
      fire_strat_rule(cutpoint,PUSH_THROUGH,0,NOSQUARE,NOT_URGENT);
      if(controlled_nbrs == 2)fire_strat_rule(cutpoint,CONNECT_THRU,0,NOSQUARE,NOT_URGENT);
      }
   else if(can_cut && cntype[cn] == CN_HANE){
      if(controlled_nbrs)fire_strat_rule(cutpoint,CUT_FROM_PEEP,ctnbval,NOSQUARE,NOT_URGENT);
      fire_strat_rule(cutpoint,CUT_HANE,0,NOSQUARE,NOT_URGENT);
      }
   }


/* double diagonal pattern.  Can connect or cut */





int crawlunder[] = { 0,0,50,150,300,50,0,0,0,0,0,0,0,0,0,0 };
int crawlover[] = { 0,0,50,200,100,50,0,0,0,0,0,0,0,0,0,0,0 };
int getlibs[] = { 0,50,150,75,25 };
int takelibs[] = { 0,50,200,100,50 };

/* I saw a segmentation fault in this routine */

/* make blocking move when stones contact diagonally 
 * Get BLOCKING_MOVE at point with two liberties and
 * enemy stones at right angles.
 * If blocking would make an empty triangle then skip instead.
 * If blocking along the edge and have enough liberties then 
 * skip instead
 */


void block_move(void){
    int s,i,ldtmp,sn,val,sgood,sbad,along_edge,under,toward_edge,sum;
    int empty_triangle,toward_friend,toward_enemy,separate_enemy;
    int enemy_behind;
    for(s = firstsquare; s < lastsquare; s++){
       if(ld[s] != NEUTRALLD || lnbn[s] < 2)continue;
       sum = 0;
	sgood = sbad = NOSQUARE;
       i = fdir[s];
       for(ldtmp = ldir[i]; i < ldtmp; ++i){
          sn = s + nbr[i];
          if(grcolor[board[sn]] == tm)sgood = sn;
          if(grcolor[board[sn]] == 1-tm)sbad = sn;
          if(board[sn] != NOGROUP)sum += nbr[i];
          }

       if(sum == 0)continue;
       if(gralive[board[sgood]] == DEAD || gralive[board[sbad]] == DEAD)
          continue;
       if(gralive[board[sgood]] == SMOTHERED || 
              gralive[board[sbad]] == SMOTHERED)
              continue;
       if(gralive[board[sbad]] > WEAK && ahead <= 2)continue;
       if(gralive[board[sgood]] == HAS_TWO_EYES && gralive[board[sbad]] > WEAK)
	       continue; 
       fire_strat_rule(s,BLOCKING_MOVE,0,NOSQUARE,NOT_URGENT);
       if(grthreatened[board[sgood]] || grthreatened[board[sbad]])
		continue;
       along_edge = edge[sgood] == edge[s];
       under = edge[sbad] > edge[s] || edge2[sbad] > edge2[s];
       toward_edge = edge[s] < edge[sgood];
       toward_friend = 
	       grcolor[board[s+s-sgood+sbad-s]] == tm ||
		       grcolor[board[s+s-sgood+s-sbad]] == tm &&
			       (lnbf[s+s-sbad][1-tm] != 0 ||
				lnbf[s+s-sbad][tm] == 1);
       if(edge[s+s-sgood] > 1)
	       toward_friend |= grcolor[board[s+s-sgood+s-sgood]] == tm;
       toward_enemy = lnbf[s+s-sgood][1-tm] != 0;
       if(!toward_enemy && edge[s+s-sgood] > 1)
	       toward_friend |= lnbf[s+s-sgood+s-sgood][tm] != 0;
       enemy_behind = ld[s+s-sbad] > 1 && grcolor[lgr[s+s-sbad]] == 1-tm ||
	       ld[s+s-sbad] == NEUTRALLD;
       separate_enemy = enemy_behind && grarmy[board[sbad]] != grarmy[lgr[s+s-sbad]] && grcolor[lgr[s+s-sbad]] != tm;
       sn = s + s - sbad;
       empty_triangle = board[sgood+s-sbad] == board[sgood] &&
	  board[s + s-sgood] == NOGROUP && board[s+ s-sbad] == NOGROUP &&
          lnbn[s + s - sgood] >= 3;
       if(empty_triangle)
          fire_strat_rule(s,EMPTY_TRIANGLE,0,NOSQUARE,NOT_URGENT);
       if(rterv[sn][tm] - rterv[sn][1-tm] > MAXRTVAL/2)
	       fire_strat_rule(s,BLOCK_DEF_TERR,0,NOSQUARE,NOT_URGENT);
       if(toward_friend && gralive[board[sbad]] <= WEAK){
          val = conn_val(board[sgood],lgr[s+s-sgood]);
	  if(val > 100)val = 100;
	  fire_strat_rule(s,TOWARD_FRIEND,val,NOSQUARE,NOT_URGENT);
	  if(grlibs[board[sgood]] > 3)
	     fire_strat_rule(s+s-sgood,SKIP_FOR_BLOCK,0,NOSQUARE,NOT_URGENT);
	  }
	if(separate_enemy){
		fire_strat_rule(s,BLOCK_AND_SEPARATE,100,NOSQUARE,NOT_URGENT);
		}
       if(!empty_triangle && toward_friend && grcolor[board[sbad+s-sgood]]
		== grcolor[board[sgood]])
	  fire_strat_rule(s,FILL_HOLE,0,NOSQUARE,NOT_URGENT);
       if(!empty_triangle){
          if(grlibs[board[sgood]] < 5){
             fire_strat_rule(s,GET_MORE_LIBS,getlibs[grlibs[board[sgood]]],NOSQUARE,NOT_URGENT);
	     }
          if(grlibs[board[sbad]] < 5 && gralive[board[sbad]] <= WEAK){
             fire_strat_rule(s,TAKE_AWAY_LIBS,takelibs[grlibs[board[sbad]]],NOSQUARE,NOT_URGENT);
	     }
          }
       if(along_edge && under){
          if(empty_triangle){
             fire_strat_rule(s+s-sgood,SKIP_FROM_MT_TRI,crawlunder[edge[s]],NOSQUARE,NOT_URGENT);
	     if(edge2[s] > edge2[sgood])
		     fire_strat_rule(s+s-sgood,OUT_OF_CORNER,0,NOSQUARE,NOT_URGENT);
             }
          else if(gralive[board[sbad]] <= WEAK){
	     val = crawlunder[edge[s]];
	     if(ld[s+s-sbad] >=3 && ld[s+s-sbad] != NOLD && 
		board[s+s-sbad+s-sgood])val = 50;
             fire_strat_rule(s,CRAWL_UNDER,val,NOSQUARE,NOT_URGENT);
	     if(edge2[s] > edge2[sgood])
		     fire_strat_rule(s+s-sgood,OUT_OF_CORNER,0,NOSQUARE,NOT_URGENT);
	     }
          if((edge[s] == 2 || edge[s] == 3) && grlibs[board[sgood]] > 4 &&
             lnbn[s+s-sgood] == 4){
	     fire_strat_rule(s+s-sgood,SKIP_ALONG_EDGE,0,NOSQUARE,NOT_URGENT);
	     if(edge2[s] > edge2[sgood])
		     fire_strat_rule(s+s-sgood,OUT_OF_CORNER,0,NOSQUARE,NOT_URGENT);
             }
          }
       else if(along_edge && !under){
          if(empty_triangle){
	     fire_strat_rule(s+s-sgood,SKIP_FROM_MT_TRI,crawlover[edge[s]],NOSQUARE,NOT_URGENT);
             }
	  else if(gralive[board[sbad]] <= WEAK)
	     fire_strat_rule(s,CRAWL_OVER,crawlover[edge[s]],NOSQUARE,NOT_URGENT);
          }
       else if(toward_edge && edge[s] < 4 && !enemy_behind){
	       fire_strat_rule(s,DEFEND_EDGE_TERR,edge[s]*50,NOSQUARE,NOT_URGENT);
	       }
       else if(!toward_edge && edge[s] < 5)
          fire_strat_rule(s,BLOCK_AWAY,0,NOSQUARE,NOT_URGENT);
       else if(empty_triangle){
	    fire_strat_rule(s+s-sgood,SKIP_FROM_MT_TRI,100,NOSQUARE,NOT_URGENT);
          }
       }
   }


void filldame(void){
   int i,ptr,j,ldtmp,sn,s,s1,s2,tmplist=EOL;

   for(i = 0; i < maxgr; ++i){
      if(!grlv[i])continue;
      if(grcolor[i] == tm && gralive[i] <= ALIVE)
         for(ptr = grlbp[i]; ptr != EOL; ptr = links[ptr]){
            s = list[ptr];
            if(ld[s] == NEUTRALLD){
		  if(gralive[lgr[s]] <= WEAK && addlist(s,&tmplist)){
                  	fire_strat_rule(s,FILLDAME_NEUTRAL,lnbn[s],NOSQUARE,NOT_URGENT);
			}
                  }
            else {
               j = fdir[s];
               for(ldtmp = ldir[j]; j < ldtmp; ++j){
                  sn = s + nbr[j];
		  if(grlibs[board[sn]] == 2){
			s1 = list[grlbp[board[sn]]];
			s2 = list[links[grlbp[board[sn]]]];
			if(ld[s1] == NEUTRALLD && gralive[lgr[s1]] != DEAD &&
				!grthreatened[lgr[s1]] && eyepot[eyerec[s2]] == 0 ||
			   ld[s2] == NEUTRALLD && gralive[lgr[s2]] != DEAD &&
				!grthreatened[lgr[s2]] && eyepot[eyerec[s1]] == 0)
			   fire_strat_rule(s,FILLDAME_CONNECT,0,NOSQUARE,NOT_URGENT);
			}
                  if(ld[sn] != NOLD && ld[sn] != NEUTRALLD && 
                     grcolor[lgr[sn]] != grcolor[lgr[s]] &&
                     gralive[lgr[sn]] <= WEAK){
                     if(lnbn[sn] > 1){
                        fire_strat_rule(sn,FILLDAME,0,NOSQUARE,NOT_URGENT);
                        }
                     else {
                        fire_strat_rule(s,FILLDAME,0,NOSQUARE,NOT_URGENT);
                        }
                     }
                  }
               }
            }
      }
   killist(&tmplist);
   }


       


int get_reasons_for_moves(void){
   int i,g;
   nextstrat = 0;
   for(i = firstsquare; i < lastsquare; ++i){
	strattotal[i] = 0;
	stratguess[i] = 0;
	stratgoodreasons[i] = FALSE;
	urgent[i] = 0;
	killist(&stratreasons[i]);
	}
   for(g = 0; g < maxgr; ++g){
	if(!grlv[g])continue;
	groldalive[g] = gralive[g];
	groldthreatened[g] = grthreatened[g];
	}
   if(!problemflag){
   	if(empty_corner())return(TRUE);	        /* play in empty corner */
	joseki();       	/* play joseki moves */
	shimari_kakari();	/* play shimari and kakari */
	close_corner();			/* close a corner after 4-4 point play */
	}
   savegroup();				/* save weak group */
   killgroup();				/* attack enemy group */
   cut_connect();			/* cut or connect */
   if(!problemflag){
   	extend();			/* extend along edge */
	center();			/* play in center */
	if(ahead > 2)play_safe();       /* ahead, so play it safe */
	if(ahead < 2 && phase != FUSEKI)
		squirm();               /* behind, so try to stage upset */
	}
   evalshapes();			/* look for shapes to suggest moves */
   contactfight();			/* respond to tsuke */
   block_move();			/* block when enemy stones diagonal */
   if(kosquare != NOSQUARE)		/* make ko threat */
      kothreat();
   if(phase == ENDGAME)
	   filldame();				/* fill dame in endgame */
   if(takestones)remove_dead_stones(); /* take dead stones off board */
   return(FALSE);
   }


void squirm(void){
	int g,ptr;
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		if(grcolor[g] != tm && (gralive[g] == 5 || gralive[g] == 6 ||
					gralive[g] == 8 || gralive[g] == 9))
			play_inside(g);
		if(grcolor[g] != tm && gralive[g] == DEAD)
			for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
				if(gralive[list[ptr]] > WEAK){
					squirm_capture(g);
					break;
					}
		if(grcolor[g] == tm && gralive[g] > WEAK && 
		   gralive[g] != DEAD && armyeyespace[grarmy[g]] >= 8)
			save_weak_army(grarmy[g]);
		}
	}

void play_inside(int g){
	int ptr,c;
	c = grcolor[g];
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr])
		if(lnbn[list[ptr]] == 1 && lnbf[list[ptr]][1-c] == 0 && 
		   lnbn[list[nblbp[list[ptr]]]] > 1 &&
		   lnbf[list[nblbp[list[ptr]]]][1-c] == 0)
			fire_strat_rule(list[nblbp[list[ptr]]],BEHIND_VITAL,
					0,NOSQUARE,NOT_URGENT);
	}

int bugfixIndex(int index);    // sm:

/* play safe when you are ahead */

void play_safe(void){
	int g,g2,ptr,tmplist = EOL,tmplist2,ptr2,ptr3,def,slist,caplist = EOL;

	if(msptr > 10){
		g = board[ bugfixIndex(mvs[msptr-1]) ];
		if(grcolor[g] == 1-tm && gralive[g] > WEAK && !grthreatened[g])
				safe_capture(grarmy[g],100);
		}

	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;


                /* capture DEAD cutting stones if you don't have an eye */
		if(grcolor[g] != tm &&
		   gralive[g] == DEAD &&
		   grlibs[g] > 1)
			for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
				g2 = list[ptr];
				if(grcnp[g2] == EOL &&
				   gralive[g2] <= 6 &&
				    !hasaneye(g2) || gralive[g2] == 8 || 
				   gralive[g2] == 9 || gralive[g2] == 10)
					addlist(grarmy[g],&caplist);
				}
		
		if(grcolor[g] != tm && gralive[g] >= WEAK_POTENTIAL && gralive[g] < 22 &&
		   armylibs[grarmy[g]] > 3)
			addlist(grarmy[g],&caplist);


                /* make eye shape for groups without definite eyes */
		if(grcolor[g] == tm && (gralive[g] == 6 || gralive[g] == 4 || 
					gralive[g] == 8 || gralive[g] == 9)){
			addlist(grarmy[g],&tmplist);
			}

		}

	for(ptr = caplist; ptr != EOL; ptr = links[ptr])
		safe_capture(list[ptr],20*armysize[list[ptr]]);

	killist(&caplist);

	for(ptr = tmplist; ptr != EOL; ptr = links[ptr]){
		slist = EOL;  /* allow moves that save two groups at once */
		def = def_val(list[ptr]);

		for(ptr2 = armypot[list[ptr]]; ptr2 != EOL; ptr2 = links[ptr2]){
			if(pots[list[ptr2]].pot_val == 0)continue;
			tmplist2 = adpot(list[ptr],list[ptr2]);
			for(ptr3 = tmplist2; ptr3 != EOL; ptr3 = links[ptr3])
				if(addlist(list[ptr3],&slist))
				   fire_strat_rule(list[ptr3],SAFE_STRENGTHEN_GROUP,def,NOSQUARE,NOT_URGENT);
			killist(&tmplist2);
			}

		make_eye_shape(list[ptr],def,FALSE,&slist);

		killist(&slist);
		}
	killist(&tmplist);
	}

void squirm_capture(int g){
	int ptr;
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr])
		fire_strat_rule(list[ptr],SQUIRM_CAPTURE,0,NOSQUARE,NOT_URGENT);
	}


void safe_capture(int army,int v){
	int ptr,val;
	val = armylibs[army]*25;
	for(ptr = armylbp[army]; ptr != EOL; ptr = links[ptr])
		fire_strat_rule(list[ptr],SAFE_CAPTURE,val+v,NOSQUARE,NOT_URGENT);
	}

int hasaneye(int g){
	int ptr;
	for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr])
		if(eyerec[list[ptr]] != 0 && eyeval[eyerec[list[ptr]]] >= 8)
			return(TRUE);
	return(FALSE);
	}

void center(void){
	int s,c,ptr;
	for(s = 0; s < boardsquare; s++){
		if(board[s] != NOGROUP)continue;
		if(ld[s] != NOLD && ld[s] != NEUTRALLD && ld[s] < 4 &&
		   gralive[lgr[s]] <= ALIVE){
			c = grcolor[lgr[s]];
			if(rterv[s][1-c] > MAXRTVAL/4)
				for(ptr = nblbp[s]; ptr != EOL; ptr = links[ptr])
					if(ld[list[ptr]] == NOLD &&
					   rterv[list[ptr]][c] < rterv[list[ptr]][1-c])
						fire_strat_rule(list[ptr],INTO_ENEMY_TERR,0,NOSQUARE,NOT_URGENT);
			}
		if(edge[s] <= 4)continue;
		if(lgr[s] != NOGROUP)continue;
		if(abs(rterv[s][1] - rterv[s][0]) < MAXRTVAL/4 && isjunc(s))
			fire_strat_rule(s,PLAY_IN_CENTER,0,NOSQUARE,NOT_URGENT);
		}
	}

int isjunc(int s){
	int i,ldtmp,pos,neg,sn;
	pos = neg = FALSE;
	if(rterv[s][0] > rterv[s][1])neg = TRUE;
	if(rterv[s][0] < rterv[s][1])pos = TRUE;
	i = fdir[s];
	for(ldtmp = ldir[i]; i < ldtmp; ++i){
		sn = s + nbr[i];
		if(ld[sn] != NOLD)continue;
		if(rterv[sn][0] > rterv[sn][1])neg = TRUE;
		if(rterv[sn][0] < rterv[sn][1])pos = TRUE;
		} 
	return(neg && pos);
	}


int threethreepoint(int cn){
	int x,y;
	x = 2;
	y = 2;
	if(cn > 1)y = boardsize-1-y;
	if(cn == 1 || cn == 3)x = boardsize-1-x;
	return(y*boardsize+x);
	}

void close_corner(void){
	int s,cn,ptr,sn,g,val;
	for(cn = 0; cn < 4; cn++){
		s = fourfourpoint(cn);
		g = board[s];
		if(g == NOGROUP){
			s = threethreepoint(cn);
			if(lnbn[s] >= 3 && lnbf[s][tm] == 0 && ahead < 2 &&
			   (boardsize <= 13 || msptr > 50))
				fire_strat_rule(s,TRY_33_INV,0,NOSQUARE,NOT_URGENT);
			continue;
			}
		if(grsize[g] != 1 || grlibs[g] != 4)continue;
		if(grcolor[g] == 1-tm){
			if(jflag[cn] > 2){
				val = 0;
				if(ahead < 2)val = 100;
				s = threethreepoint(cn);
				if(ld[s] == NOLD)
					fire_strat_rule(s,TRY_33_INV,val,NOSQUARE,NOT_URGENT);
				else if(lnbn[s] == 3 && ahead == 0)
					fire_strat_rule(s,TRY_33_INV,val,NOSQUARE,NOT_URGENT);
				}
			continue;
			}
		val = 0;
		if(ahead > 2)val = 200;
		for(ptr = grlbp[g]; ptr != EOL; ptr = links[ptr]){
			sn = list[ptr];
			if(edge[sn] > 3 && ld[sn] > 2 && ld[sn] != NOLD && ld[sn + (s-sn)*2] == 2)
				fire_strat_rule(sn + (s-sn)*2,TAKE_CORNER,val,NOSQUARE,NOT_URGENT);
			if(ltrgd[sn] && edge[sn] == 3)
				fire_strat_rule(sn,PROTECT_CORNER,val,NOSQUARE,NOT_URGENT);
			}
		}
	}


/* don't look at bad moves */

int isbadmove(int s){
	int cn;
/* skip illegal moves */

        if(s == kosquare){
		fire_strat_rule(s,ILLEGAL_KO,BAD_MOVE,NOSQUARE,NOT_URGENT);
		return(TRUE);
		}
        if(board[s] != NOGROUP){
		fire_strat_rule(s,ILLEGAL_STONE,BAD_MOVE,NOSQUARE,NOT_URGENT);
		return(TRUE);
		}

/* skip bad moves */


/* skip nonjoseki moves if found joseki move in corner */
	for(cn = 0; cn < 4; ++cn)
		if(incorner(s,cn) && foundjoseki[cn] && notjoseki(s)){
			fire_strat_rule(s,NOT_JOSEKI,BAD_MOVE,NOSQUARE,NOT_URGENT);
			return(TRUE);
			}

	return(FALSE);
   	}

int incorner(int s,int cn){
	int mid,x,y;
	mid = boardsize/2;
	x = xval[s];
	if(cn == 1 || cn == 3)x = boardsize-x;
	y = yval[s];
	if(cn > 1)y = boardsize-y;
	return(x <= mid && y <= mid && x+y < mid+mid-3);
	}

int movestotry[NUMSQUARES];
int nextmvtotry;  /* total number of moves suggested */
int lastmvtotry;  /* number of good moves that will actually be examined */
int numurgentmoves; /* number nonprescored moves that are urgent */

void sortnextmove(void){
	int i,flag = TRUE,tmp;
	while(flag){
		flag = FALSE;
		for(i = 1; i < nextmvtotry; ++i)
			if(stratguess[movestotry[i-1]] < stratguess[movestotry[i]] && urgent[movestotry[i-1]] == urgent[movestotry[i]] || !urgent[movestotry[i-1]] && urgent[movestotry[i]]){
				tmp = movestotry[i-1];
				movestotry[i-1] = movestotry[i];
				movestotry[i] = tmp;
				flag = TRUE;
				}
		}
	}


/* return TRUE if there are prescored urgent moves */

int getmovestotry(void){
	int s,tmp=0,hu = FALSE;
	nextmvtotry = numurgentmoves = 0;
	for(s = 0; s < boardsquare; ++s)
		if(stratreasons[s] != EOL){
		        stratguess[s] += tmp;
			if(scoreval[s] != BIGNUM){
				scoreval[s] += tmp;
				if(urgent[s]){
					fire_strat_rule(s,URGENT_MOVE,0,NOSQUARE,NOT_URGENT);
					hu = TRUE;
					}
				}
			else {
				if(urgent[s]){
					numurgentmoves++;
					fire_strat_rule(s,URGENT_MOVE,0,NOSQUARE,NOT_URGENT);
					}
				movestotry[nextmvtotry++] = s;
				}
		        }
	lastmvtotry = nextmvtotry;
	if(playlevel < 50 && lastmvtotry > playlevel) lastmvtotry = playlevel;
	if(playlevel < 50 && (nextmvtotry > playlevel || numurgentmoves)){
		sortnextmove();
		}
	return(hu);
	}

/* try move s and put value in scoreval. */
/* return TRUE if move is better than pass */

int tryamove(int s,int passps,int passts,int passthrs){
	int ps,ts,thrs,goodmove=FALSE;

	if(stratreasons[s] == EOL)
		return(goodmove);  /* no reason for move */	
	if(isbadmove(s))return(goodmove);
	mvs[msptr] = s;
	if(!update(msptr,TRUE)){
		fire_strat_rule(s,NO_SUICIDE,BAD_MOVE,NOSQUARE,NOT_URGENT);
		dndate(msptr);
		return(goodmove);
		}
	++msptr;
	tm = 1 - tm;
	life();
	stval(s);  /* see if rules applied */
	if(!stratgoodreasons[s]){
		fire_strat_rule(s,NO_GOOD_REASON,0,NOSQUARE,NOT_URGENT);
		}
	else {
		getscore();
		ps = pscr; 
		ts = tscr + ltrscr + rtscr;
		thrs = thrscr;
		if(tm == WHITE){
			scr = - scr;
			ps = -ps;
			ts = -ts;
			thrs = -thrs;
			}

		if(scr > passval){
			scoreval[s] = scr-passval;
			goodmove = TRUE;
			}
		else
			fire_strat_rule(s,WORSE_THAN_PASS,0,NOSQUARE,NOT_URGENT);
		}
	--msptr;
	dndate(msptr);
	tm = mvcolor[msptr];
	return(goodmove);
	}


int bestmove(void){
	int s,sqr = PASS,val = 0;;
	for(s = firstsquare; s < lastsquare; ++s){
		if(scoreval[s] == BIGNUM)continue;
		if(scoreval[s] > val){
			val = scoreval[s];
			sqr = s;
			}
		}
	return(sqr);
	}


/* strategy figures out strategic concepts that wil be used later in
 * evaluating moves.
 * it sets ahead to show if side to move is ahead or behind
 * it sets phase showing which pase the game is in.
 */

void strategy(void){
	int i,g,val;

	phase = -1;

	for(i = 0; i < 4; ++i)if(jflag[i] == 1){
		phase = FUSEKI;
		break;
		}

	if(phase == -1 && msptr > boardsquare/3){
		phase = ENDGAME;
		for(g = 0; g < maxgr; ++g){
			if(!grlv[g])continue;
			if(gralive[g] > ALIVE && gralive[g] < WEAK &&
			   grsize[g] + grlibs[g] > 5){
				phase = MIDDLE;
				break;
				}
			}
		if(msptr > boardsquare/2)phase = ENDGAME;
		}
	if(phase == -1)phase = MIDDLE;
			

	ahead = 2;  /* even score */
	val = scr;
	if(tm == BLACK)val = -val;
	if(val > 2000 && val-thrscr > 2000)ahead = 4;  /* way ahead */
	else if(val > 1000 && val-thrscr > 1000)ahead = 3;  /* ahead */
	else if(val < -1000)ahead = 0;  /* way behind */
	else if(val < 0 || val-thrscr < 0)ahead = 1;  /* behind */
	if(boardsize == 9){
		if(val > 500)ahead = 4;
		if(val > 250)ahead = 3;
		if(val < -250)ahead = 0;
		}
	fixurgdefarmies();
	}

void fixurgdefarmies(void){
	int g,ptr,army;
	killist(&urgdefarmies);
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		if(grcolor[g] != tm)continue;
		if(!thalive[gralive[g]&31])continue;
		army = grarmy[g];
		if(armyrn_pot[army] >= EASY_RUN)continue;
		if(grsize[g] <= 2 && grlibs[g] == 1 && 
		   eyepot[eyerec[mvs[grpieces[g]]]] == 0)continue;
		if(armysize[army] >= 10){
			addlist(army,&urgdefarmies);
			continue;
			}
		if(armysize[army] == 1 && edge2[mvs[grpieces[g]]] <= 4 &&
		   grlibs[g] == 4){
			addlist(army,&urgdefarmies);  /* defend corner */
			continue;
			}
		if(armynbp[army] == EOL)getarmynbp(army);
		for(ptr = armynbp[army]; ptr != EOL; ptr = links[ptr]){
			if(gralive[list[armygroups[list[ptr]]]] <= WEAK &&
			   gralive[list[armygroups[list[ptr]]]] >= UNSETTLED_RUN){
				addlist(army,&urgdefarmies);
				break;
				}
			}
		}
	}

/* compmove generates a computer move.  returns TRUE if it found
 * a move.  False if it was aborted 
 */


int compmove(void){
   int s,nm;
   int passps,passts,passthrs,moves,haveurgent;

   strategy();

   for(s = 0; s < boardsquare; s++)scoreval[s] = BIGNUM;

   mvcolor[msptr] = tm;

   nummoves = 0;
   numnodes = 0;
   tm = 1-tm;       /* update for pass */
   mvs[msptr] = PASS;
   update(msptr,TRUE);
   ++msptr;
   if(kosave[msptr-1] != NOSQUARE)
	   life();  /* need life since a pass can change aliveness values  (ko)*/
   getscore();          /* evaluate pass */

   --msptr;
   dndate(msptr);
   tm = 1-tm;       /* dndate pass */


   passval = scr;
   passps = pscr; /* + (numpris[1] - numpris[0])*50; */
   passts = ltrscr + rtscr + tscr;
   passthrs = thrscr;

   if(tm == BLACK){
	passval = -passval;
	passps = -passps;
	passts = -passts;
	passthrs = -passthrs;
	}

   bstscr = scr;

   get_reasons_for_moves();  

   haveurgent = getmovestotry();  /* sort moves that don't already have scores */


   moves = 0;
   mvcolor[msptr] = tm;
   for(nm = 0; nm < nextmvtotry; ++nm){  /* evaluate moves that don't already have scores */

	   if(haveurgent && nm >= numurgentmoves)break;
	   if(tryamove(movestotry[nm],passps,passts,passthrs)){
		   ++moves;
		   if(urgent[movestotry[nm]]){
			   haveurgent = TRUE;
			   }
		   }
	   if(moves >= lastmvtotry)break;
	   }

   mvs[msptr] = bestmove();  /* find best score */

   killist(&urgdefarmies);
   return(TRUE);
   }
  
  

/* bad move return true if any rules fired had negative values */

int badmove(int s){
	int ptr;
	for(ptr = stratreasons[s]; ptr != EOL; ptr = links[ptr])
		if(rules[strat[list[ptr]].reason].value < 0)return(TRUE);
	return(FALSE);
	}
  
/* see if the last move at s killed any enemy groups
 * (changed their aliveness from unsettled to weak)
 * value for pieces weakened or killed is already included in
 * pfac.  Add extra for liberties of group killed.  If attacking group
 * is not strong, reduce the value of the attack
 */

void check_killed(int s){
	int g,val,armylist=EOL,ptr;
	if(badmove(s))return;
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		if(grcolor[g] == grcolor[board[s]])continue;
		if(gralive[g] <= UNSETTLED)continue;
		if(groldalive[g] > UNSETTLED)continue;
		if(groldalive[g] <= VERY_ALIVE)continue;
		if(groldalive[g] <= ALIVE && ahead > 2)continue;
		addlist(grarmy[g],&armylist);
		}
	for(ptr = armylist; ptr != EOL; ptr = links[ptr]){
		g = list[armygroups[list[ptr]]];
		val = kval[gralive[g]&31] * armylibs[list[ptr]];
		if(val > 300)val = 300;
		if(gralive[board[s]] > ALIVE)val /= 2;
		if(gralive[g] == DEAD)
		    fire_strat_rule(s,KILL_UNSETTLED_GROUP,0,NOSQUARE,NOT_URGENT);
		else /* if(!grthreatened[board[s]]) */
		    fire_strat_rule(s,THRT_UNSETTLED_GROUP,val,NOSQUARE,NOT_URGENT);
                strat[list[stratreasons[s]]].goodrule = TRUE;
		}
	killist(&armylist);
	}

/* see if g (which is a DEAD group) is inside very weak groups
 * which means that g is actually alive!
 */

int deadinsidedead(int g){
	int ptr;
	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
		if(gralive[list[ptr]] <= WEAK)
			return(FALSE);
	if(grnbp[g] == EOL)return(FALSE);
	return(TRUE);
	}

/* see if we have any tactically threatened, unsettled,
 * or weak groups on the board 
 * Penalty for unsettled and weak groups is reducing value of
 * stone to -40.  Liberty values are not reduced.
 * except that the side to move gets his biggest unsettled group saved
 * (gets 40 added to his score for each piece and liberty)
 * if unsettled group is also tactically threatened, add another 50 points
 * per liberty since will lose those points too containing it.
 * c is color to move
 * if we have tactically captured groups inside very weak groups, they are 
 * actually alive so add back two points
 */

int check_threatened(int c){
	int g,usval=0,dval=0,armylist=EOL,ptr,val;
	int biggestsave = 0;  /* biggest friendly group can be saved */
	int biggestkill = 0;  /* biggest enemy group can kill */
	int army;
	for(g = 0; g < maxgr; g++){
		if(!grlv[g])continue;
		army = grarmy[g];
		if(gralive[g] == DEAD && deadinsidedead(g)){
			dval += grsize[g] * 100 * cfac[grcolor[g]];
			}
		if(grcolor[g] == c){
			if(armysize[army]+armylibs[army] > biggestsave &&
			   (thalive[gralive[g]&31] || grthreatened[g] &&
			    gralive[g] < WEAK))
				biggestsave = armysize[army]+armylibs[army];
			continue;
			}
		if(grthreatened[g] && gralive[g] <= WEAK ||
			thalive[gralive[g]&31]){
			addlist(grarmy[g],&armylist);
			}
		}
	for(ptr = armylist; ptr != EOL; ptr = links[ptr]){
		g = list[armygroups[list[ptr]]];
		val = (armylibs[list[ptr]] + armysize[list[ptr]]) * (40 + pfac[gralive[g]&31]);
		if(val > biggestkill)
			biggestkill = val;
		usval += val;
		}
	killist(&armylist);
	return((biggestsave*40+usval) * cfac[tm] + dval);
	}






/* see if last move at s is sente and add in value for it 
 * sente if move created a new threatened or unsettled group and
 * the piece just put down is not threatened and the threatened group
 * was alive before.
 * value of sente is half the value of capturing the group since will
 * take another move to capture.
 * return the value of sente
 */

int check_sente(int s){
	int g,val,cutval,thval,sval;
	val = cutval = sval = 0;
	thval = BIGNUM;
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		if(grcolor[g] == grcolor[board[s]]){
			if(groldalive[g] == DEAD && gralive[g] < WEAK &&
			   grthreatened[g])sval += 50 * grsize[g];
			continue;
			}
		if(grthreatened[g] && !groldthreatened[g] && 
		    groldalive[g] <= WEAK_LIMP){
			if((50+pfac[groldalive[g]]) * grsize[g] < thval)
				thval = (50+pfac[groldalive[g]])*grsize[g];
			}
		else if(thalive[gralive[g]&31] && 
		   gralive[board[s]] <= ALIVE && 
		   groldalive[g] <= ALIVE){
		   

			val += (50 + pfac[groldalive[g]]) * armysize[grarmy[g]] / 2;
			
			if(groldalive[g] <= ALIVE && grsize[g] > 1)
				val += 25*grsize[g];
			cutval += cut_stones_th_val(g,s); 
			/* see if cutting stones */
			}
		}
	if(thval == BIGNUM)thval = 0;
	return(sval + val+cutval+thval/2);
	}

/* value of threatening to capture cutting stones, even if can't actually
 * capture them
 */

int cut_stones_th_val(int g,int s){
	int cnval,ptr,ptr2,g1,g2,gs;
	gs = board[s];
	cnval = 0;
	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr])
		for(ptr2 = links[ptr]; ptr2 != EOL; ptr2 = links[ptr2]){
			g1 = list[ptr];
			g2 = list[ptr2];
			if(g1 == gs || g2 == gs)continue;
			if(grthreatened[g1] || grthreatened[g2])
				continue;
			if(grarmy[g1] != grarmy[g2]){
				cnval += conn_val(g1,g2);
				}
			}
	return(cnval/2);
	}






/* return TRUE if rule rl applied to move made at s 
 * rule applied if group made by move is at least as alive as rules[].weakest
 * AND
 * if rules[].attack == ATTACK, the group at sqr must have gotten weaker
 * if rules[].attack == DEFEND, the group at sqr must have gotten stronger
 */

int rule_applied(int rl,int s,int sqr){
	int g;
	g = board[s];
	if(grthreatened[g] && rules[rl].weakest != DEAD)return(FALSE);
	if(gralive[g] > rules[rl].weakest)return(FALSE);
	if(sqr == NOSQUARE || board[sqr] == NOGROUP)return(TRUE);
	if(rules[rl].attack == ATTACK && 
	   groldalive[board[sqr]] >= gralive[board[sqr]])return(FALSE);
	if(rules[rl].attack == DEFEND && 
	   groldalive[board[sqr]] <= gralive[board[sqr]])return(FALSE);
	return(TRUE);
	}



/* strategic value of this move just made
 * s is square just moved onto
 */


void stval(int s){
    	int g,ptr,s1,s2,c,sente_val,rule;

   	g = board[s];


	/* strategies for new group added */

   	if(grsize[g] == 1 ){

		/* penalty for putting new dead group on board */

		if(grthreatened[g] || gralive[g] == DEAD)
			new_th_group(g,s);
      		}



	/* block expansion along edge */

	if(gralive[g] <= VERY_ALIVE && ( edge[s] == 3 || edge[s] == 2) &&
	   fdir[s] > 32){
	   c = grcolor[g];
	   s1 = s + nbr[fdir[s]];		/* left */
	   s2 = s + nbr[fdir[s]+1];	/* right */
	   if(grcolor[board[s2]] == 1-c && groldalive[board[s2]] <= WEAK &&
	      ltr1[s1] != 0 && !ltrgd[s1] ||
	      grcolor[board[s1]] == 1-c && groldalive[board[s1]] <= WEAK &&
	      ltr1[s2] != 0 && !ltrgd[s2])
		fire_strat_rule(s,BLOCK_FROM_TERR,0,NOSQUARE,NOT_URGENT);
	      /* block out of good territory */
	   }



	/* see if any rules applied */

	for(ptr = stratreasons[s]; ptr != EOL; ptr = links[ptr]){
		rule = strat[list[ptr]].reason;
		if(rule_applied(rule,s,strat[list[ptr]].param)){
			strat[list[ptr]].goodrule = TRUE;
			if(rules[rule].value >= 0){
				stratgoodreasons[s] = TRUE;
				}
			}
		else if(strat[list[ptr]].urgent)urgent[s]--;
		}
	
	if(stratgoodreasons[s])
		check_killed(s);	/* killed anything by accident? */

	sente_val = 0;
	if(stratgoodreasons[s] && !grthreatened[g] && gralive[g] != DEAD)
	   	sente_val = check_sente(s);	/* is this move sente? */

	/* add up strategic values */
	for(ptr = stratreasons[s]; ptr != EOL; ptr = links[ptr]){
		rule = strat[list[ptr]].reason;
		if(strat[list[ptr]].goodrule)
			strattotal[s] += strat[list[ptr]].value + rules[rule].value;
		}


	if(stratgoodreasons[s] && strattotal[s] > 0 && sente_val > 0){

		if(sente_val > 500)sente_val = 500;
		fire_strat_rule(s,SENTE_THREAT_MOVE,sente_val,NOSQUARE,NOT_URGENT);
                strat[list[stratreasons[s]]].goodrule = TRUE;
		strattotal[s] += sente_val;
		}

   	}



/* started new threatened or DEAD group, find strategic value
 * bad to start new dead or threatened group unless it
 * kills a neighboring group (throw in)
 * makes a ko threat
 * captures some stones
 * you are behind
 */

void new_th_group(int g,int s){
	int ptr,deadflag,thflag,thsize,thlibs;
	deadflag = FALSE;	/* dead neighboring group */
	thflag = FALSE;		/* threatened neighboring group */
	thsize = thlibs = 0;
	for(ptr = grnbp[g]; ptr != EOL; ptr = links[ptr]){
		if(gralive[list[ptr]] == SEMEAI ||
		   gralive[list[ptr]] >= WEAK_POTENTIAL)deadflag = TRUE;
		if(grthreatened[list[ptr]]){
			thflag = TRUE;
			thsize += grsize[list[ptr]];
			thlibs += grlibs[list[ptr]];
			}
		}
	if(kosave[msptr-1] != NOSQUARE && thflag)
		fire_strat_rule(s,KOTHREAT,75 * thsize + 25 * thlibs,NOSQUARE,NOT_URGENT);
	else if(!deadflag && mvcapt[msptr-1] == EOL && ahead > 1)
		fire_strat_rule(s,NO_TH_GROUPS,-200 - 100 * grsize[g] - 100 * thlibs,NOSQUARE,NOT_URGENT);
	}


int stfac[] = { 0,0,0,0,0,0,0,0,0,0,  0,0,-30,-50,-75,-100,
                  -150,-200,-250,-350,  -450,-600,-600,-600,-50,-50 };
  


  
int getscore(void){ 
   int s;
   stscr = 0;
   if(msptr > 0){
      s = mvs[msptr - 1];
      if(s != PASS){
         stscr = strattotal[s] * cfac[mvcolor[msptr-1]]; 
         }
      }
   cntterr();
   scr = stscr; 	/* strategic value of move */
   thrscr = check_threatened(tm);
	/* subtract penalty for threatened or unsettled groups on board */
   scr += thrscr;
   if(!problemflag)
      scr += tscr + ltrscr + rtscr;	/* territory value */
   scr += pscr + komi; /* piece value */
   pclsnext = 0;
   evcolor = tm;
   return(scr); 
   }
  


/* radiated territory.  Each stone radiates influence based on aliveness
 * of stone and taxicab distance from stone
 * value doesn't count if already counted in ltrscr or tscr
 */
                        /* one value for each distance and aliveness value */
int rtval1[12][26];	/* for alive groups */
int rtval2[12][26];     /* for weak and unsettled groups */

int rtalive1[] = {	/* maximum value for rt for each aliveness */
0,0,
100,100,100,100,90,	/* very alive */
98,95,93,90,	/* miai for life */
0,0,0,0,	/* unsettled */
0,0,0,0,
0,0,0,0,
0,0,-1 };

int rtalive2[] = {	/* maximum value for rt for each aliveness */
0,0,
0,0,0,0,0,	/* very alive */
0,0,0,0,	/* miai for life */
0,80,50,50,	/* unsettled */
70,60,30,0,
0,0,0,0,
0,0,0 };

# define MAXDIST 8

/* influence falls off linearly with distance since circumference increases
 * linearly with radius.  Use 2 times 1/dist so area only needs to be
 * 50% surrounded to be secure
 */

int rtfactor = 2;  /* factor to multiply rtdist by */

int rtthreshold;  /* if one side is over rtthreshold, other side gets no
                     credit for territory. Set at distance 4 since group
		     can do a one point jump. */

int d1i[] = {
1,19,-1,-19 };

int d2i[] = {
19,-1,-19,1 };

int d1[4],d2[4],d3[4]; /* (d3 is sum of d1 and d2) */

int maxsharedrt = MAXRTVAL * 10 / 7;

void initrtval(void){
	int dist,alive,i,mdist,wdist;
	rtthreshold = 3 * rtfactor * MAXRTVAL / 4;
	mdist = MAXDIST;
	if(playlevel < 20)mdist = playlevel/4+2;
	wdist = MAXDIST;
	if(playlevel < 20)wdist = playlevel/4+1;
	for(alive = 0; alive <= 25; alive++){
		for(dist = 1; dist <= mdist; dist++){
			rtval1[dist][alive] = MAXRTVAL * rtalive1[alive] * rtfactor / 100 / 4 / dist;
			if(playlevel < 50 && rtval1[dist][alive] < MAXRTVAL/32 &&
			   rtval1[dist][alive] > -MAXRTVAL/32)rtval1[dist][alive] = 0;
			}
		for(dist = 1; dist <= wdist; ++dist){
			rtval2[dist][alive] = MAXRTVAL * rtfactor * rtalive2[alive] / 100 / 4 / dist;
			if(playlevel < 50 && rtval2[dist][alive] < MAXRTVAL/32 &&
			   rtval2[dist][alive] > -MAXRTVAL/32)rtval2[dist][alive] = 0;
			}
		}
	for(i = 0; i < 4; ++i){
		if(d1i[i] == 19)d1[i] = boardsize;
		else if(d1i[i] == -19)d1[i] = -boardsize;
		else d1[i] = d1i[i];
		if(d2i[i] == 19)d2[i] = boardsize;
		else if(d2i[i] == -19)d2[i] = -boardsize;
		else d2[i] = d2i[i];
		d3[i] = d1[i] + d2[i];
		}
	}

/* NOTE: THE FOLLOWING CODE REQUIRES 32 BIT INTEGERS TO WORK PROPERLY!!! */
/* ONLY IF MAXRTVAL != 100 */


void radiateterr(void){
	int g,gral,ptr;
	radiateclear();
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		gral = gralive[g];
		if(gral >= 32)continue;  /* doesn't have aliveness yet */
		if(rtval1[1][gral] == 0)continue;
		for(ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr])
			if(lnbn[mvs[ptr]] != 0 && edge[mvs[ptr]] > 1)
				radiatepiece(mvs[ptr],gral,grcolor[g],rtval1);
		}
	radiatecorner(0,1,boardsize);
	radiatecorner(boardsize-1,-1,boardsize);
	radiatecorner(boardsquare-1,-1,-boardsize);
	radiatecorner(boardsquare-boardsize,1,-boardsize);
	}


void radiateweak(void){
	int g,gral,ptr;
	for(g = 0; g < maxgr; ++g){
		if(!grlv[g])continue;
		gral = gralive[g];

		if(rtval2[1][gral] == 0)continue;
		for(ptr = grpieces[g]; ptr != -1; ptr = mvnext[ptr])
			if(lnbn[mvs[ptr]] != 0 && edge[mvs[ptr]] > 1)
				radiatepiece(mvs[ptr],gral,grcolor[g],rtval2);
		}
	
	radiatescore();
	}
	


void radiateclear(void){
	int i;
	for(i = 0; i < boardsquare; ++i)rterv[i][0] = rterv[i][1] = 0;
	}

void radiatescore(void){
	int tval,i; 
	rtscr = 0;
	for(i = 0; i < boardsquare; ++i){
		if(!ltrgd[i] && ltr1[i]){ /* square counted as edge territory */
			continue;
			}
		if(ld[i] != NOLD){ /* square counted as next to piece territory */
			continue;
			}
		if(rterv[i][1] > rtthreshold && rterv[i][0] > rtthreshold)tval = 0;
		else if(rterv[i][1] > maxsharedrt && rterv[i][0] > 0)
			tval = maxsharedrt-rterv[i][0];
		else if(rterv[i][0] > maxsharedrt && rterv[i][1] > 0)
			tval = rterv[i][1] - maxsharedrt;
		else if(rterv[i][1] > MAXRTVAL)tval = MAXRTVAL - rterv[i][0];
		else if(rterv[i][0] > MAXRTVAL)tval = rterv[i][1] - MAXRTVAL;
		else tval = rterv[i][1] - rterv[i][0];
		if(edge[i] <= 1)tval *= 2;
		if(edge[i] == 2)tval = tval * 2 / 3;
		if(tval < 5 && tval > -5)tval = 0;
		if(tval > MAXRTVAL)tval = MAXRTVAL;
		if(tval < -MAXRTVAL)tval = -MAXRTVAL;
		rtscr += tval;
		}
	rtscr = rtscr/2;  /* WARNING!!! DEPENDS ON MAXRTVAL == 100!!! */
               /* required for working with 16 bit integers */
	}


/* do influence for corner squares */

void radiatecorner(int s,int dir1,int dir2){
	int i;
	if(board[s] != NOGROUP)return;
	for(i = 0; i < 2; ++i){
		rterv[s][i] += (rterv[s+dir1][i] + rterv[s+dir2][i])/2;
		}
	}


void radiatepiece(int s,int gral,int c,int (*rtval)[26]){
/*int s,gral,c,rtval[12][26]; {*/
	int dir,sn,dist,count1,count2,point1[50],point2[50],i,sn2,inc,rt;
	for(dir = 0; dir < 4; ++dir){
	
		/* radiate along lines */
		inc = d1[dir];
		sn = s + inc;
		dist = 1;
		rt = rtval[dist][gral];
		while(board[sn] == NOGROUP &&
		      rt != 0){
			rterv[sn][c] += rt;
			if(dist > 1 && ld[sn] != NOLD)break;  
                           /* don't radiate thru walls */
			if(edge[sn] < 2)break;
			sn += inc;
			++dist;
			rt = rtval[dist][gral];
			}

		/* do quadrants */

		if(board[s+d1[dir]] != NOGROUP && board[s+d2[dir]] != NOGROUP)
			continue;

		dist = 2;
		sn = s + d3[dir];
		if(board[sn] != NOGROUP)continue;
		rterv[sn][c] += rtval[dist][gral];
		if(edge[sn] < 2)continue;
		count1 = 1;
		point1[0] = sn;

		while(1){
			if(count1 == 0)break;  /* no more points to expand */
			++dist;
			rt = rtval[dist][gral];
			if(rt == 0)break;  /* no point in going on*/
			count2 = 0;
			
			/* do top of first piece */
			sn = point1[0] + d1[dir];
			if(board[sn] == NOGROUP){
				rterv[sn][c] += rt;
				if(edge[sn] > 1 && 
				   ld[sn] == NOLD)
					point2[count2++] = sn;
				}

			/* do diags of middle */
			for(i = 1; i < count1; ++i){
				sn = point1[i-1] + d2[dir];
				if(board[sn] == NOGROUP){
					rterv[sn][c] += rt;
					if(edge[sn] > 1 && 
					   ld[sn] == NOLD)
						point2[count2++] = sn;
					}
				sn2 = point1[i] + d1[dir];
				if(sn2 == sn)continue;
				if(board[sn2] == NOGROUP){
					rterv[sn2][c] += rt;
					if(edge[sn2] > 1 &&
					   ld[sn2] == NOLD)
						point2[count2++] = sn2;
					}
				}

			/* do bottom of bottom piece */
			sn = point1[count1-1] + d2[dir];
			if(board[sn] == NOGROUP){
				rterv[sn][c] += rt;
				if(edge[sn] > 1 && 
				   ld[sn] == NOLD)
					point2[count2++] = sn;
				}

			/* loop unrolled 2 times for speed (saves copying pointers) */

			if(count2 == 0)break;  /* no more points to expand */
			++dist;
			rt = rtval[dist][gral];
			if(rt == 0)break;  /* no point in going on*/
			count1 = 0;
			
			/* do top of first piece */
			sn = point2[0] + d1[dir];
			if(board[sn] == NOGROUP){
				rterv[sn][c] += rt;
				if(edge[sn] > 1 && 
				   ld[sn] == NOLD)
					point1[count1++] = sn;
				}

			/* do diags of middle */
			for(i = 1; i < count2; ++i){
				sn = point2[i-1] + d2[dir];
				if(board[sn] == NOGROUP){
					rterv[sn][c] += rt;
					if(edge[sn] > 1 &&
					   ld[sn] == NOLD)
						point1[count1++] = sn;
					}
				sn2 = point2[i] + d1[dir];
				if(sn2 == sn)continue;
				if(board[sn2] == NOGROUP){
					rterv[sn2][c] += rt;
					if(edge[sn2] > 1 &&
					   ld[sn2] == NOLD)
						point1[count1++] = sn2;
					}
				}

			/* do bottom of bottom piece */
			sn = point2[count2-1] + d2[dir];
			if(board[sn] == NOGROUP){
				rterv[sn][c] += rt;
				if(edge[sn] > 1 &&
				   ld[sn] == NOLD)
					point1[count1++] = sn;
				}
			}
		}
	}




/*
int clibs[] = { 0,-80, -50, -20, 05, 30,50, 70,080 };
*/
/*
int afac[] = {0,0,8,8,8,7,7,6,6,5 ,5,3,0,-2,-5,-12,-25,-35,-40,-40,
                 -45,-55,-65,-75,-190,-190,-190,-190 };
*/
  
/* evaluate the territory in liberties */
  
  
void cntterr(void){
   int s,g,sn,i,k,j,ldtmp,ldtm2,sn2;
   int tmplist,ptr;
  
   tmplist = EOL;  
   for(i = 0; i != pclsnext; ++i){
      s = pcls[i];
      if(ld[s] == 0){
         mrglist(grlbp[board[s]],&tmplist);
         tscr -= terv[s]; 
         terv[s] = 0; 
         }
      else if(ld[s] == NOLD){
         tscr -= terv[s]; 
         terv[s] = 0; 
	 }
      else
         addlist(s,&tmplist); 
      j = fdir[s];
      for(ldtmp = ldir[j]; j != ldtmp; ++j){
         sn = s + nbr[j]; 
         if(ld[sn] == 0){ 
            tscr -= terv[sn]; 
            terv[sn] = 0; 
	    mrglist(grlbp[board[sn]],&tmplist);
            continue;
            } 
         else if(ld[sn] == NOLD){ 
            tscr -= terv[sn]; 
            terv[sn] = 0; 
            } 
         else 
            addlist(sn,&tmplist); 
         k = fdir[sn];
         for(ldtm2 = ldir[k]; k != ldtm2; ++k){ 
            sn2 = sn + nbr[k];
            if(sn2 == s)continue;
            if(ld[sn2] == 0){
               tscr -= terv[sn2]; 
               terv[sn2] = 0; 
               mrglist(grlbp[board[sn2]],&tmplist);
               }
            else if(ld[sn2] == NOLD){
               tscr -= terv[sn2]; 
               terv[sn2] = 0; 
               }
            else
               addlist(sn2,&tmplist); 
            } 
         }
      if(tmplist != EOL){
         mrglist(tmplist,&terhd);
         killist(&tmplist);
         }
      } 
  
   for(ptr = chgrp; ptr != EOL; ptr = links[ptr]){
      g = list[ptr];
      mrglist(grlbp[g],&terhd); 
      } 
   k = terhd; 
   for(k = terhd; k != EOL; k = links[k]){ 
            s = list[k];

            tscr -= terv[s];
            terv[s] = 0;
            ltrscr -= ltr2[s];
            ltr2[s] = 0;
	    if(ld[s] != NOLD)
		    evallibsterr(s);
	    }
   killist(&terhd);
   killist(&chgrp);
   }




/* figure out how much territory a liberty is worth.  50 = 1 point.
 * first find the deadest neighboring group.
 * keep track of if neighboring groups are equally dead.
 * if the liberty is edge territory, adjust for the aliveness and return.
 * if point is neutral, give to enemy if owning group dead.
 * if point completey surrounded and nbr group has 2 liberties and
 * the other liberty is neutral, then this is not a point.
 */


void evallibsterr(int s){
	int g,v,gral,c,c1,eflag,l,sn,ldtmp;

        g = lgr[s]; 
        v = ld[s];
        if(g == NOGROUP || v == 0){  /* not a liberty */
               ltr1[s] = 0;
               ltrgd[s] = FALSE;
               return; 
               } 
        gral = gralive[g];

        c = cfac[grcolor[g]]; 
        c1 = c;         /* color of deadest group */
        eflag = FALSE;  /* equally alive groups of opposite color */
        if(v > 3 || v == NEUTRALLD){   /* set gral to deadest group */
               l = fdir[s]; 
               for(ldtmp = ldir[l]; l != ldtmp; ++l){ 
                  sn = s + nbr[l];
                  if(ld[sn] != 0)continue;
                  if(gralive[lgr[sn]] == gral && grcolor[lgr[sn]] 
                     != grcolor[lgr[s]])eflag = TRUE; 

                  if(grthreatened[lgr[sn]] && gral != DEAD && !grthreatened[lgr[s]] ||
		     grthreatened[lgr[sn]] == grthreatened[lgr[s]] &&
		     gralive[lgr[sn]] > gral){
                     gral = gralive[lgr[sn]]; 
                     eflag = FALSE; 
                     lgr[s] = lgr[sn];
                     c1 = cfac[grcolor[lgr[s]]];
                     }
                  } 
               }

	if(ltr1[s] != 0){  /* edge territory */
        	if(!ltrgd[s]){
               		ltrscr += ltr1[s] * ltrfac[gral] * c1;
               		ltr2[s] = ltr1[s] * ltrfac[gral] * c1;
			return;
               		}
		}

        if(v == NEUTRALLD){  /* neutral territory */
               	if(eflag)return;
               	if(ltrfac[gral] > 0)return;
		sc = ltrfac[gral] * c1;
		tscr += sc;
		terv[s] = sc;
		return;
               	}

        sc = (ltrfac[gral]*c1*dfac[v])/8;
        tscr += sc;
        terv[s] = sc;
        }
  
  
