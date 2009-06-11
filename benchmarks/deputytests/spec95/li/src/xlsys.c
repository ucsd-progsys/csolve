/* xlsys.c - xlisp builtin system functions */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xload - direct input from a file */
NODE *xload(NODE *args)
{
    NODE ***oldstk,*fname __HEAPIFY,*val;
    int vflag,pflag;
    char *name = 0;//BUG: uninitialized variable is used if xlfail returns

    /* create a new stack frame */
    oldstk = xlsave1(&fname);

    /* get the file name, verbose flag and print flag */
    fname = xlarg(&args);
    vflag = (args ? xlarg(&args) != NIL : TRUE);
    pflag = (args ? xlarg(&args) != NIL : FALSE);
    xllastarg(args);

    /* get the filename string */
    if (symbolp(fname))
	name = getstring(getpname(fname));
    else if (stringp(fname))
	name = getstring(fname);
    else
/* this code was 	MJP
	xlfail("bad argument type",fname);
*/
	xlfail("bad argument type");

    /* load the file */
    val = (xlload(name,vflag,pflag) ? true : NIL);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the status */
    return (val);
}

/* xgc - xlisp function to force garbage collection */
NODE *xgc(NODE *args)
{
    /* make sure there aren't any arguments */
    xllastarg(args);

    /* garbage collect */
    gc();

    /* return nil */
    return (NIL);
}

/* xexpand - xlisp function to force memory expansion */
NODE *xexpand(NODE *args)
{
    int n,i;

    /* get the new number to allocate */
    n = (args ? getfixnum(xlmatch(INT,&args)) : 1);
    xllastarg(args);

    /* allocate more segments */
    for (i = 0; i < n; i++)
	if (!addseg())
	    break;

    /* return the number of segments added */
    return (cvfixnum((FIXNUM)i));
}

/* xalloc - xlisp function to set the number of nodes to allocate */
NODE *xalloc(NODE *args)
{
    int n,oldn;

    /* get the new number to allocate */
    n = getfixnum(xlmatch(INT,&args));

    /* make sure there aren't any more arguments */
    xllastarg(args);

    /* set the new number of nodes to allocate */
    oldn = anodes;
    anodes = n;

    /* return the old number */
    return (cvfixnum((FIXNUM)oldn));
}

/* xmem - xlisp function to print memory statistics */
NODE *xmem(NODE *args)
{
    /* make sure there aren't any arguments */
    xllastarg(args);

    /* print the statistics */
    stats();

    /* return nil */
    return (NIL);
}

/* xtype - return type of a thing */
NODE *xtype(NODE *args)
{
    NODE *arg;

    if (!(arg = xlarg(&args)))
	return (NIL);

    switch (ntype(arg)) {
	case SUBR:	return (a_subr);
	case FSUBR:	return (a_fsubr);
	case LIST:	return (a_list);
	case SYM:	return (a_sym);
	case INT:	return (a_int);
	case FLOAT:	return (a_float);
	case STR:	return (a_str);
	case OBJ:	return (a_obj);
	case FPTR:	return (a_fptr);
	case VECT:	return (a_vect);
	default:	xlfail("bad node type");
    }
    /*NOTREACHED*/
}

/* xbaktrace - print the trace back stack */
NODE *xbaktrace(NODE *args)
{
    int n;

    n = (args ? getfixnum(xlmatch(INT,&args)) : -1);
    xllastarg(args);
    xlbaktrace(n);
    return (NIL);
}

/* xexit - get out of xlisp */
NODE *xexit(NODE *args)
{
    xllastarg(args);
    osfinish ();
    exit(0);
	return NIL; /* The trouble with C is to put functions in a table
			they all have to be the same type MJP */
}

