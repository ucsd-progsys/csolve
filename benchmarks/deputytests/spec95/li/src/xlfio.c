/* xlfio.c - xlisp file i/o */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xread - read an expression */
NODE *xread(NODE *args)
{
    NODE ***oldstk,*fptr __HEAPIFY,*eof __HEAPIFY,*rflag,*val;

    /* create a new stack frame */
    oldstk = xlsave2(&fptr,&eof);

    /* get file pointer and eof value */
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdin));
    eof = (args ? xlarg(&args) : NIL);
    rflag = (args ? xlarg(&args) : NIL);
    xllastarg(args);

    /* read an expression */
    if (!xlread(fptr,&val,rflag != NIL))
	val = eof;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the expression */
    return (val);
}

/* xprint - built-in function 'print' */
NODE *xprint(NODE *args)
{
    return (printit(args,TRUE,TRUE));
}

/* xprin1 - built-in function 'prin1' */
NODE *xprin1(NODE *args)
{
    return (printit(args,TRUE,FALSE));
}

/* xprinc - built-in function princ */
NODE *xprinc(NODE *args)
{
    return (printit(args,FALSE,FALSE));
}

/* xterpri - terminate the current print line */
NODE *xterpri(NODE *args)
{
    NODE *fptr;

    /* get file pointer */
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdout));
    xllastarg(args);

    /* terminate the print line and return nil */
    xlterpri(fptr);
    return (NIL);
}

/* printit - common print function */
LOCAL NODE *printit(NODE *args,int pflag,int tflag)
{
    NODE ***oldstk,*fptr __HEAPIFY,*val __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave2(&fptr,&val);

    /* get expression to print and file pointer */
    val = xlarg(&args);
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdout));
    xllastarg(args);

    /* print the value */
    xlprint(fptr,val,pflag);

    /* terminate the print line if necessary */
    if (tflag)
	xlterpri(fptr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xflatsize - compute the size of a printed representation using prin1 */
NODE *xflatsize(NODE *args)
{
    return (flatsize(args,TRUE));
}

/* xflatc - compute the size of a printed representation using princ */
NODE *xflatc(NODE *args)
{
    return (flatsize(args,FALSE));
}

/* flatsize - compute the size of a printed expression */
LOCAL NODE *flatsize(NODE *args,int pflag)
{
    NODE ***oldstk,*val __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave1(&val);

    /* get the expression */
    val = xlarg(&args);
    xllastarg(args);

    /* print the value to compute its size */
    xlfsize = 0;
    xlprint(NIL,val,pflag);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the length of the expression */
    return (cvfixnum((FIXNUM)xlfsize));
}

/* xopeni - open an input file */
NODE *xopeni(NODE *args)
{
    return (openit(args,"r"));
}

/* xopeno - open an output file */
NODE *xopeno(NODE *args)
{
    return (openit(args,"w"));
}

/* openit - common file open routine */
LOCAL NODE *openit(NODE *args,char *mode)
{
    NODE *fname,*val;
    char *name = 0;  //BUG: uninitialized variable is used if xlfail returns

    FILE *fp;

    /* get the file name */
    fname = xlarg(&args);
    xllastarg(args);

    /* get the name string */
    if (symbolp(fname))
	name = getstring(getpname(fname));
    else if (stringp(fname))
	name = getstring(fname);
    else
/* this code was 	MJP
	xlfail("bad argument type",fname);
*/
	xlfail("bad argument type");

    /* try to open the file */
    if ((fp = fopen(name,mode)) != NULL)
	val = cvfile(fp);
    else
	val = NIL;

    /* return the file pointer */
    return (val);
}

/* xclose - close a file */
NODE *xclose(NODE *args)
{
    NODE *fptr;

    /* get file pointer */
    fptr = xlmatch(FPTR,&args);
    xllastarg(args);

    /* make sure the file exists */
    if (getfile(fptr) == NULL)
	xlfail("file not open");

    /* close the file */
    fclose(getfile(fptr));
    setfile(fptr,NULL);

    /* return nil */
    return (NIL);
}

/* xrdchar - read a character from a file */
NODE *xrdchar(NODE *args)
{
    NODE *fptr;
    int ch;

    /* get file pointer */
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdin));
    xllastarg(args);

    /* get character and check for eof */
    return ((ch = xlgetc(fptr)) == EOF ? NIL : cvfixnum((FIXNUM)ch));
}

/* xpkchar - peek at a character from a file */
NODE *xpkchar(NODE *args)
{
    NODE *flag,*fptr;
    int ch;

    /* peek flag and get file pointer */
    flag = (args ? xlarg(&args) : NIL);
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdin));
    xllastarg(args);

    /* skip leading white space and get a character */
    if (flag)
	while ((ch = xlpeek(fptr)) != EOF && isspace(ch))
	    xlgetc(fptr);
    else
	ch = xlpeek(fptr);

    /* return the character */
    return (ch == EOF ? NIL : cvfixnum((FIXNUM)ch));
}

/* xwrchar - write a character to a file */
NODE *xwrchar(NODE *args)
{
    NODE *fptr,*chr;

    /* get the character and file pointer */
    chr = xlmatch(INT,&args);
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdout));
    xllastarg(args);

    /* put character to the file */
    xlputc(fptr,(int)getfixnum(chr));

    /* return the character */
    return (chr);
}

/* xreadline - read a line from a file */
NODE *xreadline(NODE *args)
{
    NODE ***oldstk,*fptr __HEAPIFY,*str __HEAPIFY,*newstr;
    int len,blen,ch;
    char *p,*sptr;

    /* create a new stack frame */
    oldstk = xlsave2(&fptr,&str);

    /* get file pointer */
    fptr = (args ? xlgetfile(&args) : getvalue(s_stdin));
    xllastarg(args);

    /* get character and check for eof */
    len = blen = 0; p = buf;
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n') {

	/* check for buffer overflow */
	if (blen >= STRMAX) {
 	    newstr = newstring(len+STRMAX);
	    sptr = getstring(newstr); *sptr = 0;
	    //if (str) strlcat(sptr,getstring(str),len+STRMAX);
	    if (str) {
              strncpy(sptr,getstring(str),len+STRMAX);
            }
	    //*p = 0; strlcat(sptr,buf,len+STRMAX);
	    *p = 0; strncat(sptr,buf,STRMAX);
            sptr[len+STRMAX] = 0;
	    p = buf; blen = 0;
	    len += STRMAX;
	    str = newstr;
	}

	/* store the character */
	*p++ = ch; blen++;
    }

    /* check for end of file */
    if (len == 0 && p == buf && ch == EOF) {
	xlstack = oldstk;
	return (NIL);
    }

    /* append the last substring */
    if (str == NIL || blen) {
	newstr = newstring(len+blen);
	sptr = getstring(newstr); *sptr = 0;
/* 	if (str) strlcat(sptr,getstring(str),len+blen+1); */
/* 	*p = 0; strlcat(sptr,buf,len+blen+1); */
        sptr[len+blen] = 0;
	if (str) strncpy(sptr,getstring(str),len+blen);
	*p = 0; strncat(sptr,buf,blen);
	str = newstr;
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the string */
    return (str);
}

