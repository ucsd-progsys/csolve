/* xlstr - xlisp string builtin functions */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xstrcat - concatenate a bunch of strings */
NODE *xstrcat(NODE *args)
{
    NODE ***oldstk,*val __HEAPIFY,*p;
    char *str;
    int len;

    /* create a new stack frame */
    oldstk = xlsave1(&val);

    /* find the length of the new string */
    for (p = args, len = 0; p; )
	len += strlen(getstring(xlmatch(STR,&p)));

    /* create the result string */
    val = newstring(len);
    str = getstring(val);
    *str = 0;

    /* combine the strings */
    while (args) {
      char* argstr = getstring(xlmatch(STR,&args));
      strncat(str,argstr,strlen(argstr)+1);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new string */
    return (val);
}

/* xsubstr - return a substring */
NODE *xsubstr(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*src __HEAPIFY,*val __HEAPIFY;
    int start,forlen,srclen;
    char *srcptr,*dstptr;

    /* create a new stack frame */
    oldstk = xlsave3(&arg,&src,&val);

    /* initialize */
    arg = args;
    
    /* get string and its length */
    src = xlmatch(STR,&arg);
    srcptr = getstring(src);
    srclen = strlen(srcptr);

    /* get starting pos -- must be present */
    start = getfixnum(xlmatch(INT,&arg));

    /* get length -- if not present use remainder of string */
    forlen = (arg ? getfixnum(xlmatch(INT,&arg)) : srclen);

    /* make sure there aren't any more arguments */
    xllastarg(arg);

    /* don't take more than exists */
    if (start + forlen > srclen)
	forlen = srclen - start + 1;

    /* if start beyond string -- return null string */
    if (start > srclen) {
	start = 1;
	forlen = 0; }
	
    /* create return node */
    val = newstring(forlen);
    dstptr = getstring(val);

    /* move string */
    for (srcptr += start-1; forlen--; *dstptr++ = *srcptr++)
	;
    *dstptr = 0;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the substring */
    return (val);
}

/* xstring - return a string consisting of a single character */
NODE *xstring(NODE *args)
{
    /* get the character (integer) */
    buf[0] = getfixnum(xlmatch(INT,&args));
    xllastarg(args);

    /* make a one character string */
    buf[1] = 0;
    return (cvstring(buf));
}

/* xchar - extract a character from a string */
NODE *xchar(NODE *args)
{
    char *str;
    int n;

    /* get the string and the index */
    str = getstring(xlmatch(STR,&args));
    n = getfixnum(xlmatch(INT,&args));
    xllastarg(args);

    /* range check the index */
    if (n < 0 || n >= strlen(str))
	xlerror("index out of range",cvfixnum((FIXNUM)n));

    /* return the character */
    return (cvfixnum((FIXNUM)str[n]));
}

