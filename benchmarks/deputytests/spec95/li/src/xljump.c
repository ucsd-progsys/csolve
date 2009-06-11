/* xljump - execution context routines */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xlbegin - beginning of an execution context */
void xlbegin(CONTEXT *cptr,int flags,NODE *expr)
{
    cptr->c_flags = flags;
    cptr->c_expr = expr;
    cptr->c_xlstack = xlstack;
    cptr->c_xlenv = xlenv;
    cptr->c_xltrace = xltrace;
    cptr->c_xlcontext = xlcontext;
    xlcontext = cptr;
}

/* xlend - end of an execution context */
void xlend(CONTEXT *cptr)
{
    xlcontext = cptr->c_xlcontext;
}

/* xljump - jump to a saved execution context */
void xljump(CONTEXT *cptr,int type,NODE *val)
{
    /* restore the state */
    xlcontext = cptr;
    xlstack = xlcontext->c_xlstack;
    xlenv = xlcontext->c_xlenv;
    xltrace = xlcontext->c_xltrace;
    xlvalue = val;

    /* call the handler */
    longjmp(xlcontext->c_jmpbuf,type);
}

/* xltoplevel - go back to the top level */
void xltoplevel(void)
{
    findtarget(CF_TOPLEVEL,"no top level");
}

/* xlcleanup - clean-up after an error */
void xlcleanup(void)
{
    findtarget(CF_CLEANUP,"not in a break loop");
}

/* xlcontinue - continue from an error */
void xlcontinue(void)
{
    findtarget(CF_CONTINUE,"not in a break loop");
}

/* xlgo - go to a label */
void xlgo(NODE *label)
{
    CONTEXT *cptr;
    NODE *p;

    /* find a tagbody context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_GO)
	    for (p = cptr->c_expr; consp(p); p = cdr(p))
		if (car(p) == label)
		    xljump(cptr,CF_GO,p);
    xlfail("no target for GO");
}

/* xlreturn - return from a block */
void xlreturn(NODE *val)
{
    CONTEXT *cptr;

    /* find a block context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_RETURN)
	    xljump(cptr,CF_RETURN,val);
    xlfail("no target for RETURN");
}

/* xlthrow - throw to a catch */
void xlthrow(NODE *tag,NODE *val)
{
    CONTEXT *cptr;

    /* find a catch context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if ((cptr->c_flags & CF_THROW) && cptr->c_expr == tag)
	    xljump(cptr,CF_THROW,val);
    xlfail("no target for THROW");
}

/* xlsignal - signal an error */
void xlsignal(char *emsg,NODE *arg)
{
    CONTEXT *cptr;

    /* find an error catcher */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_ERROR) {
	    if (cptr->c_expr && emsg)
		xlerrprint("error",NULL,emsg,arg);
	    xljump(cptr,CF_ERROR,NIL);
	}
    xlfail("no target for error");
}

/* findtarget - find a target context frame */
LOCAL findtarget(int flag, char *error)
{
    CONTEXT *cptr;

    /* find a block context */
    for (cptr = xlcontext; cptr; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & flag)
	    xljump(cptr,flag,NIL);
    xlabort(error);
}

