/* xlio - xlisp i/o routines */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xlgetc - get a character from a file or stream */
int xlgetc(NODE *fptr)
{
    NODE *lptr,*cptr = 0;//BUG: uninitialized variable is used if xlfail returns
    FILE *fp;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
	ch = EOF;

    /* otherwise, check for input from a stream */
    else if (consp(fptr)) {
	if ((lptr = car(fptr)) == NIL)
	    ch = EOF;
	else {
	    if (!consp(lptr) ||
		(cptr = car(lptr)) == NIL || !fixp(cptr))
		xlfail("bad stream");
	    if (rplaca(fptr,cdr(lptr)) == NIL)
		rplacd(fptr,NIL);
	    ch = getfixnum(cptr);
	}
    }

    /* otherwise, check for a buffered file character */
    else if (ch = getsavech(fptr))
	setsavech(fptr,0);

    /* otherwise, get a new character */
    else {

	/* get the file pointer */
	fp = getfile(fptr);

	/* prompt if necessary */
	if (prompt && fp == stdin) {

	    /* print the debug level */
	    if (xldebug)
		{ sprintf(buf,"%d:",xldebug); stdputstr(buf); }

	    /* print the nesting level */
	    if (xlplevel > 0)
		{ sprintf(buf,"%d",xlplevel); stdputstr(buf); }

	    /* print the prompt */
	    stdputstr("> ");
	    prompt = FALSE;
	}

	/* get the character */
	if (((ch = osgetc(fp)) == '\n' || ch == EOF) && fp == stdin)
	    prompt = TRUE;
    }

    /* return the character */
    return (ch);
}

/* xlpeek - peek at a character from a file or stream */
int xlpeek(NODE *fptr)
{
    NODE *lptr,*cptr = 0;//BUG: uninitialized variable is used if xlfail returns
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
	ch = EOF;

    /* otherwise, check for input from a stream */
    else if (consp(fptr)) {
	if ((lptr = car(fptr)) == NIL)
	    ch = EOF;
	else {
	    if (!consp(lptr) ||
		(cptr = car(lptr)) == NIL || !fixp(cptr))
		xlfail("bad stream");
	    ch = getfixnum(cptr);
	}
    }

    /* otherwise, get the next file character and save it */
    else
	setsavech(fptr,ch = xlgetc(fptr));

    /* return the character */
    return (ch);
}

/* xlputc - put a character to a file or stream */
void xlputc(NODE *fptr,int ch)
{
    NODE ***oldstk,*lptr __HEAPIFY;

    /* count the character */
    xlfsize++;

    /* check for output to nil */
    if (fptr == NIL)
	;

    /* otherwise, check for output to a stream */
    else if (consp(fptr)) {
	oldstk = xlsave1(&lptr);
	lptr = consa(NIL);
	rplaca(lptr,cvfixnum((FIXNUM)ch));
	if (cdr(fptr))
	    rplacd(cdr(fptr),lptr);
	else
	    rplaca(fptr,lptr);
	rplacd(fptr,lptr);
	xlstack = oldstk;
    }

    /* otherwise, output the character to a file */
    else
	osputc(ch,getfile(fptr));
}

/* xlflush - flush the input buffer */
void xlflush(void)
{
    if (!prompt)
	while (xlgetc(getvalue(s_stdin)) != '\n')
	    ;
}

