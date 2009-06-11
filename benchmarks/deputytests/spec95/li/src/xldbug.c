/* xldebug - xlisp debugging support */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xlfail - xlisp error handler */
/*VARARGS*/
void xlfail(char *emsg /*sm: , ...*/)
{
    xlerror(emsg,stacktop());
}
 
/* xlabort - xlisp serious error handler */
void xlabort(char *emsg)
{
    xlsignal(emsg,s_unbound);
}

/* xlbreak - enter a break loop */
void xlbreak(char *emsg, NODE *arg)
{
    breakloop("break",NULL,emsg,arg,TRUE);
}

/* xlerror - handle a fatal error */
void xlerror(char *emsg, NODE *arg)
{
    doerror(NULL,emsg,arg,FALSE);
}

/* xlcerror - handle a recoverable error */
void xlcerror(char *cmsg,char *emsg, NODE *arg)
{
    doerror(cmsg,emsg,arg,TRUE);
}

/* xlerrprint - print an error message */
void xlerrprint(char *hdr,char *cmsg,char *emsg, NODE *arg)
{
    sprintf(buf,"%s: %s",hdr,emsg); stdputstr(buf);
    if (arg != s_unbound) { stdputstr(" - "); stdprint(arg); }
    else xlterpri(getvalue(s_stdout));
    if (cmsg) { sprintf(buf,"if continued: %s\n",cmsg); stdputstr(buf); }
}

/* doerror - handle xlisp errors */
LOCAL void /*matth: added return type */
doerror(char *cmsg,char *emsg, NODE *arg, int cflag)
{
    /* make sure the break loop is enabled */
    if (getvalue(s_breakenable) == NIL)
	xlsignal(emsg,arg);

    /* call the debug read-eval-print loop */
    breakloop("error",cmsg,emsg,arg,cflag);
}

/* breakloop - the debug read-eval-print loop */
LOCAL void breakloop(char *hdr,char *cmsg,char *emsg, NODE *arg, int cflag)
{
    NODE ***oldstk,*expr __HEAPIFY,*val;
    CONTEXT cntxt __HEAPIFY;
    int type;

    /* print the error message */
    xlerrprint(hdr,cmsg,emsg,arg);

    /* flush the input buffer */
    xlflush();

    /* do the back trace */
    if (getvalue(s_tracenable)) {
	val = getvalue(s_tlimit);
	xlbaktrace(fixp(val) ? (int)getfixnum(val) : -1);
    }

    /* create a new stack frame */
    oldstk = xlsave1(&expr);

    /* increment the debug level */
    xldebug++;

    /* debug command processing loop */
    xlbegin(&cntxt,CF_ERROR|CF_CLEANUP|CF_CONTINUE,true);
    for (type = 0; type == 0; ) {

	/* setup the continue trap */
	if (type = setjmp(cntxt.c_jmpbuf))
	    switch (type) {
	    case CF_ERROR:
		    xlflush();
		    type = 0;
		    continue;
	    case CF_CLEANUP:
		    continue;
	    case CF_CONTINUE:
		    if (cflag) {
			stdputstr("[ continue from break loop ]\n");
			continue;
		    }
		    else xlabort("this error can't be continued");
	    }

	/* read an expression and check for eof */
	if (!xlread(getvalue(s_stdin),&expr,FALSE)) {
	    type = CF_CLEANUP;
	    break;
	}

	/* evaluate the expression */
	expr = xleval(expr);

	/* print it */
	xlprint(getvalue(s_stdout),expr,TRUE);
	xlterpri(getvalue(s_stdout));
    }
    xlend(&cntxt);

    /* decrement the debug level */
    xldebug--;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* check for aborting to the previous level */
    if (type == CF_CLEANUP) {
	stdputstr("[ abort to previous level ]\n");
	xlsignal(NULL,NIL);
    }
}

/* stacktop - return the top node on the stack */
LOCAL NODE *stacktop(void)
{
    return (xltrace >= 0 && xltrace < TDEPTH?trace_stack[xltrace]:s_unbound);
}

/* baktrace - do a back trace */
void xlbaktrace(int n)
{
    int i;

    for (i = xltrace; (n < 0 || n--) && i >= 0; i--)
	if (i < TDEPTH)
	    stdprint(trace_stack[i]);
}

/* xldinit - debug initialization routine */
void xldinit(void)
{
    //sm: if ((trace_stack = (NODE **)malloc(TDEPTH * sizeof(NODE *))) == NULL) {
    if ((trace_stack = (NODE **)malloc(TDEPTH * sizeof(*trace_stack))) == NULL) {
	printf("insufficient memory");
	osfinish();
	exit(1);
    }
    //sm: total += (long)(TDEPTH * sizeof(NODE *));
    total += (long)(TDEPTH * sizeof(*trace_stack));
    xlsample = 0;
    xltrace = -1;
    xldebug = 0;
}

