/*
 * Copyright (c) 1994
 *      The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Marti Hearst.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

/*
 *  tile - partition text into convenient sections called "tiles".
 */

/*
 *  Tilize the document contained in the file.  The default tokenizer
 *  returns tokens of type TEXT_TOKEN, to denote a word, and types
 *  BLANK_TOKEN and type INDENT_TOKEN.  BLANK_TOKEN indicates a blank
 *  line, and INDENT_TOKEN indicates a line that had whitespace prefixing
 *  it.  Based on the value of a flag, you can indicate the presence of
 *  a new paragraph by either a series of one or more blank lines (the default)
 *  or he presence of an indented line, in case this document doesn't use
 *  blank lines to break paragraphs.  Anything more complicted than that
 *  and you'll have to write your own lex tokenizer to parse your document.
 *  If any token type other than these three is encountered, and you have
 *  provided a callback function, than your function will be called with the
 *  value of the token, the text of the token if any, and the file offset
 *  it was encountered at.  This could be used for example to process
 *  files that contain multiple documents, or if you want to extract
 *  information out of the processed file (and without cluttering up the
 *  the tilizing code caontained here).  Of course you will have to write
 *  your own tokenizer to process your additional tokenss, and write the
 *  the callback funtion to do something useful with the information.
 *  After the document is completely processed, a TILEDOC structure is
 *  returned that contains the fileoffsets of the recognized tiles.
 */

/* 
 * To be honest, I don't understand this algorithm.  Comments preceded with 
 * an "@" are notes to myelf about what I think the code is doing and should 
 * not be taken as statements of fact by the original author.
 */
#define GC_DEBUG

#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <errno.h>
#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/file.h>
#include "gc.h"
#include "tile.h"
#include "token.h"



#define STREQ(a, b) (strcmp(a, b)==0)

extern FILE	 *yyin;
extern int	token_type;
extern int	bytes;
extern char	*common_words[];
extern char 	*yytext;

#define  MAX_KEY_LEN        (256)	/* max term size */

/*
 * we grow these as we see more sentences in the document
 */
int	*Sentpara = (int *)NULL;
long	*Paralocs = (long *)NULL;	
double	*w1 = (double *)NULL;
double	*w2 = (double *)NULL;
int	*Sentarray = (int *)NULL;	/* wloc array converted to sent 
					   array  upon demand */
int cur_sentsize = 200;

int	wordcount;
int	max_sent;
int	max_word;
int	max_para;

typedef struct  {
	double	val;
	int	sgap;
} SORTF;

/*
 * One of these per word stored in the tcl hash table named "TP".
 */
typedef struct AInfo {
	int	current_count;	/* # times word has been seen. */
	int	para_count;	/* # of paragraphs it's appeared in. */
	int	last_para;	/* Last paragraph it appeared in (so we know
				 * when to bump para_count */
	int	*wloc; 		/* Indexed by occurence of word.  Records
				 * the sentence # it appearded in.  Remember
				 * sentences aren't real sentences, but
				 * arbitrarily every 20 words */
	int	wlocsize;	/* grow wloc as needed */
} AInfo;


char	*yylex();
void	*yyrestart(FILE *);

/*
 * Function prototypes
 */
TILEDOC *process_input();
void compute_all_sim();
void show_sentence_stats();
void compute_sim(int, int);
void show_counts();
void process_token(char *, int, int);
void align_hashkey(char *, char *);
void smooth();
TILEDOC *tile_locs();
void dobfile(char *);
void show_tile_locs();
void find_boundaries(double *);
void plot_para_boundaries(SORTF *, double);
void plot_sent_boundaries(SORTF *, double);
void show_segments();
void output_segments();
double determine_scores(double *, SORTF *);
void tile_getopts(int, char **);
int cmp(const void *, const void *);
int compsf(const void *, const void *);
void init_stopword_table();
void lower_and_stem_it(char *, char *);
int stopword(char *);
int wloc_to_sentarray(int *, int);

/*
 * Tweakable Parameters (externed in tile.h)
 */
int	bound = 2;
int	numiter = 1;
int	this_k = 4;
int	word_sep_num = 20;
int	not_para_boundaries = 0;
int	verbose = 0;
int	indented = 0;	/* process indented text as a paragraph */

long	startoftext, endoftext;

#ifdef lint
#define printf (void)printf
#define fprintf (void)fprintf
#define sprintf (void)sprintf
#define strcpy	(void)strcpy
#define strncpy	(void)strncpy
#define strcat	(void)strcat
#endif

/*******************************************************************************/
/* Hash function taken from histok. */
#define HASHMAX 200

int hash(char *str)
{
    int k;

    for (k=0; *str; str++) {
	k = (k>>1) | (k<<7&0x80);
	k += *str;
    }
    return k%HASHMAX;
}


typedef struct stop_hash_entry {
  struct stop_hash_entry *next;
  char *name;
} stop_hash_entry;

typedef struct ai_hash_entry {
  struct ai_hash_entry *next;
  char *name;
  AInfo *tip;
} ai_hash_entry;

static stop_hash_entry *stop_bucket[HASHMAX];
static ai_hash_entry *ai_bucket[HASHMAX];

void stopword_add(char *str)
{
  int key;
  stop_hash_entry *p;
  
  key = hash(str);
  for (p=stop_bucket[key]; p && !STREQ(str, p->name); p=p->next);
  if (!p) {
	p = (stop_hash_entry *)GC_MALLOC(sizeof(stop_hash_entry));
	p->next = stop_bucket[key];
	stop_bucket[key] = p;
	p->name = (char *)GC_MALLOC_ATOMIC(strlen(str)+1);
	strcpy(p->name, str);
  }
}

void init_stopword_table()
{
  int key, i;
  for (key=0; key<HASHMAX; key++)
    stop_bucket[key] = NULL;
  
  for (i = 0; common_words[i] != NULL; i++)
    stopword_add(common_words[i]);
}

void stopword_free() {
 int key;
 stop_hash_entry *p, *q;
 for (key=0; key<HASHMAX; key++)
   {
     /*     for (p=stop_bucket[key]; p; p = q)
       {
	 q = p->next;
	 free(p);
       }
       */
     stop_bucket[key] = NULL;
   }
}

int stopword_find(char *str)
{
  int key;
  stop_hash_entry *p;
  
  key = hash(str);
  for (p=stop_bucket[key]; p && !STREQ(str, p->name); p=p->next);
  if (!p) 
    return 0;
  else
    return 1;
}



AInfo *ai_add(char *str, AInfo *a)
{
  int key;
  ai_hash_entry *p;
  
  p = (ai_hash_entry *) GC_MALLOC(sizeof(ai_hash_entry));
  key = hash(str);
  p->next = ai_bucket[key];
  ai_bucket[key] = p;
  p->name = (char *)GC_MALLOC_ATOMIC(strlen(str)+1);
  strcpy(p->name,str);
  p->tip = a;
}


void init_ai()
{
  int key, i;
  for (key=0; key<HASHMAX; key++)
    ai_bucket[key] = NULL;
}

void ai_free() {
 int key;
 ai_hash_entry *p, *q;
 for (key=0; key<HASHMAX; key++)
   {
     /*     for (p=ai_bucket[key]; p; p = q)
       {
	 q = p->next;
	 free(p->tip->wloc);
	 free(p->tip);
	 free(p->name);
	 free(p);
       }
       */
     ai_bucket[key] = NULL;
   }
}


AInfo *ai_find(char *str, int *new)
{
  int key;
  ai_hash_entry *p;
  
  *new = 0;

  key = hash(str);
  for (p=ai_bucket[key]; p && !STREQ(str, p->name); p=p->next);

  if (!p)
    {
      *new = 1;
      p = (ai_hash_entry *) GC_MALLOC(sizeof(ai_hash_entry));
      key = hash(str);
      p->next = ai_bucket[key];
      ai_bucket[key] = p;
      p->name = (char *)GC_MALLOC_ATOMIC(strlen(str)+1);
      strcpy(p->name,str);
      p->tip = (AInfo *) GC_MALLOC(sizeof(AInfo));
    }
  return p->tip;
}

TILEDOC *
tile(char *fname, void (*callback)(int, char *, long))
{
	char	buf[MAX_KEY_LEN];

	int	outwordcount = 0;
	char	*str;
	int	snum = 1;
	int	para = 1;
	int	seen_para_break = 0;
	int	i;
	TILEDOC	*td;
	static int stopinit = 0;
#define	GROWSENT 200
	/*
	 * Reset for new doc
	 */
	mksentarrays(0, cur_sentsize);
	startoftext = bytes = 0;
	max_sent = 0;
	max_para = 0;

        if ((yyin = fopen(fname, "r")) == NULL) {
                fprintf(stderr, "Can't open %s\n", fname);
                return (TILEDOC *)NULL;
        }
	if (! stopinit) { init_stopword_table(); stopinit=1;}
	init_ai();

	while (str = yylex()) {
	  if (verbose) printf("token: %s    type: %d\n", str, token_type);
                if (token_type == TEXT_TOKEN) {
                        strcpy(buf, str);
			process_token(buf, snum, para);
			outwordcount++;
			seen_para_break = 0;
			/*
			 * @ It looks like every 20 words (word_sep_num) we
			 * @ arbitrarily say we've seen another sentence. So
			 * @ it seems we don't care about periods or newlines.
			 * @ - strange...
			 */
			if ((outwordcount % word_sep_num) == 0)
				snum++;
                } else if (((token_type == BLANK_TOKEN) || 
		    (indented && (token_type == INDENT_TOKEN))) && 
		    (Sentpara[snum] == 0) && (Sentpara[snum-1] == 0)) {
                        if (seen_para_break == 0) {
                                Sentpara[snum] = para;
                                Paralocs[snum] = bytes;
                                para++;
                        }
                        seen_para_break = 1;
                } else if (token_type != NULL_TOKEN && callback != NULL) {
			(*callback)(token_type, yytext, bytes);
		}

		if (snum > cur_sentsize)
			mksentarrays(cur_sentsize, cur_sentsize += GROWSENT);
	}
	fclose(yyin);
	endoftext = bytes;
	max_sent = snum;
	max_para = para;

	/*
	 * Determine the tile locations.
	 */
	td = process_input();

	/*
	 * cleanup - prepare in case we have to process a new file
	 */
	ai_free();
	freesentarrays();
	reset_tokenizer();
	
	return (td);
}

TILEDOC *
process_input()
{
	compute_all_sim();
	smooth();
	return (tile_locs());
}

void
lower_and_stem_it(char *term, char *buffer)
{
	register int	i;

	strcpy(buffer, term);
	for (i = 0; i < strlen(term) + 1; i++)
		buffer[i] = tolower(buffer[i]);

	/* 
	 * stemming removed -- we aren't doing it anymore because it
	 * didn't seem to buy us anything.  If you want to experiment
	 * with it, you can add it back here.
	 */
#ifdef notdef
	if (!(stopword_find(buffer)))
		strcpy(buffer, mymorph(buffer));
#endif

}

	


int	
compsf(const void *t1, const void *t2)
{
	
	if (((SORTF *)t1)->val > ((SORTF *)t2)->val)
		return (-1);
	if (((SORTF *)t1)->val < ((SORTF *)t2)->val)
		return (1);

	return (0);
}

int
cmp(const void *d1, const void *d2)
{
	if (*(long *)d1 < *(long *)d2)
		return (-1);
	if (*(long *)d1 > *(long *)d2)
		return (1);

	return (0);
}

/* @ return value sometimes unused */
double
determine_scores(double *w, SORTF *scores)
{
	int	i, j, sentgap;
	double	value, score;
	double	max = 0.0;

	for (i = 0; i < max_sent; i++) {
		scores[i].val = 0.0;
		scores[i].sgap = 0;
	}

	for (sentgap = 1; sentgap < max_sent; sentgap++) {
		value = w[sentgap];
		if (w[sentgap] > max)
			max = w[sentgap];

		score = 0.0;
		j = sentgap - 1;
		while (j > 1) {
			if (w[j] < value)
				break;
			value = w[j];
			j--;
		}
		score = value - w[sentgap];
		j = sentgap + 1;
		value = w[sentgap];
		while (j < max_sent) {
			if (w[j] < value)
				break;
			value = w[j];
			j++;
		}
		if ((score > 0.0) && ((value - w[sentgap]) > 0.0)) {
			score += value - w[sentgap];
			scores[sentgap].val = score;
			scores[sentgap].sgap = sentgap;
		}
	}
	return(max);
}

/*
 * @ well, this part i don't understand (marti?)
 */
void
smooth()
{
	int	i, j, k;
	double	nw;
	int	count;

	if (verbose) {
		printf("\n\n");
		for (j = 1; j < max_sent; j++) {
			printf("**** %d %.3lf\n", j, w1[j]);
		}
	}

	for (j = 1; j < this_k; j++)
		w2[j] = w1[j];
	for (j = (max_sent - this_k); j < max_sent; j++)
		w2[j] = w1[j];
	for (k = 0; k < numiter; k++) {
		for (j = this_k; j < (max_sent - this_k); j++) {
			nw = 0.0;
			count = 0;
			for (i = (-bound + 1); i < bound; i++) {
				nw += w1[j+i];
				count++;
			}
			w2[j] = nw / (double)count;
		}
		for (j = 1; j < max_sent; j++) {
			w1[j] = w2[j];
		}
	}
#ifdef notdef
	if (verbose) {
		printf("\n\n");
		for (j = 1; j < max_sent; j++) {
			printf("%d %.3lf\n", j, w2[j]);
		}
		find_boundaries(w2);
	}
#endif
}


void
align_hashkey(char *str, char *buf)	/* @ ??? */
{
	bzero(buf, MAX_KEY_LEN);
	strncpy(buf, str, strlen(str));

}

void
compute_sim(int ss, int k)
{
	double	num, w1s, w2s, s1, s2, temp;
	int	j;
	int	ss2;
	AInfo	*ip;
	ai_hash_entry *a;
	int bucket_index;

	ss2 = ss + k;

	num = w1s = w2s = 0.0;
	for(bucket_index = 0; bucket_index < HASHMAX; bucket_index++)
	  for(a = ai_bucket[bucket_index]; a != NULL; a = a->next)
	    {
		s1 = s2 = 0.0;
		ip = a->tip;
		wloc_to_sentarray(ip->wloc, ip->current_count);
		for (j = ss; j < (ss + k); j++) {
			s1 += Sentarray[j];
		}
		for (j = ss2; j < (ss2 + k); j++) {
			s2 += Sentarray[j];
		}
		num += s1 * s2;
		w1s += s1 * s1;
		w2s += s2 * s2;
	    }
	temp = sqrt(w1s * w2s);
	w1[ss+k-1] = (temp) ?  (num / temp) : 0.0;
}

void
compute_all_sim()
{
	int	i;
	/* use a smaller amount of context at the beginning and end */
	for (i = 1; i < this_k; i++) {
		compute_sim(i, 3);
	}
	/* @ ??? - doesn't this want to start at "this_k"? */
	for (i = 1; i <= (max_sent - (this_k * 2) + 2); i++) {
		compute_sim(i, this_k);
	}
	for (i = (max_sent - this_k); i < (max_sent - 4); i++) {
		compute_sim(i, 3);
	}
}

/*
 * Convert the sentence location array (wloc) to an array indexible
 * by sentence number.
 */
wloc_to_sentarray(int *wloc, int current_count)
{
	int i;

	bzero(Sentarray, cur_sentsize * sizeof(int));

	for (i = 0; i < current_count; i++)
		Sentarray[wloc[i]]++;
}

/*
 * Record the fact we saw a term.  We use a tcl hash table for no
 * better reason than it was handy at the time.  We keep track of the
 * sentence number and paragraph number it occured in.
 */
void
process_token(char *str, int sent, int para)
{
  AInfo * infoPtr;
  int	new;
  char	term[MAX_KEY_LEN];
  char	buf[MAX_KEY_LEN];
  
  lower_and_stem_it(str, term);
  if (!stopword_find(term)) {
    align_hashkey(term, buf);	/* ??? */
    /*
     * @@@
     * Put term into hash table (key is term).
     * Keep track of which sentence each instance of this term
     * appears in.  Also, keep track of the highest numbered
     * paragrap this term appears in.  In Tcl hash tables,
     * you store pointers (or integers) called ClientData.
     */
    infoPtr = ai_find(buf,&new);
    if (new)
      {
	if ((infoPtr->wloc = (int *)GC_MALLOC_ATOMIC(sizeof(int) * 10)) 
	    == NULL) {
	  fprintf(stderr, "Out of memory\n");
	  exit(1);
	}
	infoPtr->wlocsize = 10;
	infoPtr->current_count = 0;
	infoPtr->last_para = -1;
	infoPtr->para_count = 0;
      }
    if (infoPtr->current_count >= infoPtr->wlocsize) {
      infoPtr->wlocsize += 50;
      if ((infoPtr->wloc = (int *)GC_REALLOC(infoPtr->wloc,
					  sizeof(int) * infoPtr->wlocsize)) == NULL) {
	fprintf(stderr, "Out of memory\n");
	exit(1);
      }
    }
    infoPtr->wloc[infoPtr->current_count] = sent;
    if (para > infoPtr->last_para) {
      infoPtr->last_para = para;
      (infoPtr->para_count)++;
    }
    (infoPtr->current_count)++;

    if(verbose)
      fprintf(stdout, "* wrd: %s current: %d paracnt: %d lastpapr: %d wlocsize: %d\n",
			buf, infoPtr->current_count, infoPtr->para_count,
			infoPtr->last_para, infoPtr->wlocsize);

  }
}

/*
 * Build the list of tile offset locations to be returned.
 */
TILEDOC *
tile_locs()
{
	FILE * fp;
	SORTF *scores;
	int	*used;
	register int	i, j;
	int	para, sentgap;
	long	parabyte, prev;
	int	count = 0;
	double	avg, sd, limit;
	TILEDOC	*tdp;	/* tiledoc return structure */
	TILE	*tp;	/* pointer to tilearray in TILEDOC */
	long	*bounds;
	double	*sss;

	bounds = (long *)GC_MALLOC_ATOMIC(sizeof(long) * max_para);
	if (bounds == NULL) {
		fprintf(stderr, "Out of mammarys\n");
		exit(1);
	}
	bzero(bounds, sizeof(long) * max_para);

	sss = (double *)GC_MALLOC_ATOMIC(sizeof(double) * max_para);
	if (sss == NULL) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}
	bzero(sss, sizeof(double) * max_para);

	scores = (SORTF *)GC_MALLOC(sizeof(SORTF) * max_sent);
	if (scores == NULL) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}
	bzero(scores, sizeof(SORTF) * max_sent);
	used = (int *)GC_MALLOC_ATOMIC(sizeof(int) * max_sent);
	if (used == NULL) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}
	bzero(used, sizeof(int) * max_sent);

	determine_scores(w2, scores);

	qsort(scores, max_sent, sizeof(SORTF), compsf);
	for (i = 0; i < max_sent; i++) {
		para = 0;
		sentgap = scores[i].sgap;
		parabyte = Paralocs[sentgap];
		for (j = 0; j < 6; j++) {
			if (Sentpara[sentgap+j] > 0) {
				para = Sentpara[sentgap+j];
				parabyte = Paralocs[sentgap+j];
				break;
			} else if (Sentpara[sentgap-j] > 0) {
				para = Sentpara[sentgap-j];
				parabyte = Paralocs[sentgap-j];
				break;
			}
		}
		if (scores[i].val > 0.0) {
			if ((used[para] != 1) && 
			    ((parabyte > startoftext) && (parabyte < endoftext))) {
				sss[para] = scores[i].val;
				bounds[para] = parabyte;
				count++;
			}
			used[para] = 1;
		} else {
			break;
		}
	}

	avg = 0.0; 
	sd = 0.0;
	for (i = 0; i < max_para; i++) {
		avg += sss[i];
	}
	avg = avg / (double)(count ? count : 1);
	for (i = 0; i < max_para; i++) {
		if (sss[i] > 0.0) {
			sd += ((sss[i] - avg) * (sss[i] - avg));
		}
	}
	if ((count - 1) <= 0)
		sd = 0;
	else
		sd = sqrt(sd / (double)(count - 1));
	limit = avg - (sd / 2.0);

	/* record tiles */

	/*
	 * Create the TILEDOC structure to return the tile information in.
	 * We create a maximum sized tilearray and then shrink it after
	 * filling it in.
	 */
	tdp = (TILEDOC *)GC_MALLOC(sizeof(TILEDOC));
	if (tdp == NULL) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}
	tdp->tilearray = tp = (TILE *)GC_MALLOC(sizeof(TILE) * max_para);
	if (tdp->tilearray == NULL) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}
	
	prev = startoftext;
	for (i = 0; i < max_para; i++) {
		if ((sss[i] > limit) && (bounds[i] > prev)) {
			tp->startoff = prev;
			tp->endoff = bounds[i];
			tp++;
			prev = bounds[i] + 1;
		}
	}
	tp->startoff = prev;
	tp->endoff = endoftext;
	tp++;

	tdp->numtiles = tp - tdp->tilearray;

	/* 
	 * Shrink tilearray now that we know how big it is.
	 */
	/* tdp->tilearray = realloc(tdp->tilearray, 
		((char *)tp - (char *)tdp->tilearray)); */
	/*	free(bounds);
	free(sss);
	free(scores);
	free(used);*/
	  
	return (tdp);
}

/*
 * grow the sentence arrays as needed
 */
int
mksentarrays(int old, int new)
{
	Sentpara = (int *)GC_REALLOC(Sentpara, sizeof(int) * new);
	Paralocs = (long *)GC_REALLOC(Paralocs, sizeof(long) * new);
	w1 = (double *)GC_REALLOC(w1, sizeof(double) * new);
	w2 = (double *)GC_REALLOC(w2, sizeof(double) * new);
	Sentarray = (int *)GC_REALLOC(Sentarray, sizeof(int) * new);

	if (!Sentpara || !Paralocs || !w1 || !w2 || !Sentarray) {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	}
	
	/*
	 * make sure the new part is zeroed out
	 */
        bzero(&Sentpara[old], ((int) &Sentpara[new]) - ((int) &Sentpara[old]));
        bzero(&Paralocs[old], ((int) &Paralocs[new]) - ((int) &Paralocs[old]));
        bzero(&w1[old], ((int) &w1[new]) - ((int) &w1[old]));
        bzero(&w2[old], ((int) &w2[new]) - ((int) &w2[old]));
        bzero(&Sentarray[old], ((int) &Sentarray[new]) - ((int) &Sentarray[old]));
}

/*
 * Free a TILEDOC structure.  The caller of tile() calls this to
 * free the space if desired.
 */
freetiledoc(TILEDOC *tdp)
{
  /*	if (tdp) {
		if (tdp->tilearray)
			free(tdp->tilearray);
		free(tdp);
	}*/
}

/*
 * Cleanup storage we used
 */
freesentarrays()
{
	if (Sentpara) {
	  /*		free(Sentpara);*/
		Sentpara = NULL;
	}
	if (Paralocs) {
	  /*		free(Paralocs);*/
		Paralocs = NULL;
	}
	if (w1) {
	  /*		free(w1);*/
		w1 = NULL;
	}
	if (w2) {
	  /*		free(w2);*/
		w2 = NULL;
	}
	if (Sentarray) {
	  /*		free(Sentarray);*/
		Sentarray = NULL;
	}
}

/*
 * and prepare the tokenizer for the next file, if any.
 */
reset_tokenizer() {
	extern int lastbytes;
	extern int token_seen;

	token_seen = 0;
	lastbytes = 0;
	bytes = 0;

	lex_reset();	/* have to tell lex we may want a new file */

	/* reset other crap */
	wordcount = 0;
	max_sent = 0;
	max_word = 0;
	max_para = 0;
	startoftext = endoftext = 0;
	
}
