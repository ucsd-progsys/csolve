/* xlisp - a small implementation of lisp with object-oriented programming */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* define the banner line string */
#define BANNER  "XLISP version 1.6, Copyright (c) 1985, by David Betz"

/* main - the main routine */
int main(int argc,char **argv)
{   
    // sm: making the context static, since 'main' isn't called
    // reentrantly
    static CONTEXT cntxt;
    NODE *expr __HEAPIFY;
    int i;

    /* initialize and print the banner line */
    osinit(BANNER);

    // sm: here, and below, I noticed that almost all NODE*
    // were inferred EFAT by our tool, but the only reason
    // was the strange use of '1' to be a NULL pointer; I
    // changed this to '0', and suddenly all NODE* are SAFE!
    // this leads to a 40% performance improvement

    /* setup initialization error handler */
    #ifdef NO_PERF_CHANGES
      xlbegin(&cntxt,CF_TOPLEVEL|CF_ERROR,(NODE *) 1);
    #else
      xlbegin(&cntxt,CF_TOPLEVEL|CF_ERROR,(NODE *)0);
    #endif
    if (setjmp(cntxt.c_jmpbuf)) {
	printf("fatal initialization error\n");
	osfinish();
	exit(1);
    }

    /* initialize xlisp */
    xlinit();
    xlend(&cntxt);

    /* reset the error handler */
    #ifdef NO_PERF_CHANGES
      xlbegin(&cntxt,CF_TOPLEVEL|CF_ERROR,true);
    #else
      xlbegin(&cntxt,CF_TOPLEVEL|CF_ERROR, (NODE*)0);
    #endif

    /* load "init.lsp" */
    if (setjmp(cntxt.c_jmpbuf) == 0)
	xlload("init.lsp",FALSE,FALSE);

    /* load any files mentioned on the command line */
    if (setjmp(cntxt.c_jmpbuf) == 0)
	for (i = 1; i < argc; i++)
	    if (!xlload(argv[i],TRUE,FALSE))
		xlfail("can't load file");

    /* create a new stack frame */
    xlsave1(&expr);

    /* main command processing loop */
    while (TRUE) {

	/* setup the error return */
	if (i = setjmp(cntxt.c_jmpbuf)) {
	    if (i == CF_TOPLEVEL)
		stdputstr("[ back to the top level ]\n");
	    setvalue(s_evalhook,NIL);
	    setvalue(s_applyhook,NIL);
	    xldebug = 0;
	    xlflush();
	}

	/* read an expression */
	if (!xlread(getvalue(s_stdin),&expr,FALSE))
	    break;

	/* evaluate the expression */
	expr = xleval(expr);

	/* print it */
	stdprint(expr);
    }
    xlend(&cntxt);
    osfinish ();
    exit (0);
}

/* stdprint - print to standard output */
void stdprint(NODE *expr)
{
    xlprint(getvalue(s_stdout),expr,TRUE);
    xlterpri(getvalue(s_stdout));
}

/* stdputstr - print a string to standard output */
void stdputstr(char *str)
{
    xlputstr(getvalue(s_stdout),str);
}

