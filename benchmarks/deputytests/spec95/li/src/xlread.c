/* xlread - xlisp expression input routine */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

#define WSPACE "\t \f\r\n"
#define CONST1 "!$%&*+-./0123456789:<=>?@[]^_{}~"
#define CONST2 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

/* xlload - load a file of xlisp expressions */
int xlload(char *fname,int vflag,int pflag)
{
    NODE ***oldstk,*fptr __HEAPIFY,*expr __HEAPIFY;
    char fullname[STRMAX+1];
    CONTEXT cntxt __HEAPIFY;
    FILE *fp;
    int sts;

    /* create a new stack frame */
    oldstk = xlsave2(&fptr,&expr);

    /* create the full file name */
    if (needsextension(fname)) {
      //strlcpy(fullname,fname, STRMAX+1);
      //strlcat(fullname,".lsp", STRMAX+1);
	strncpy(fullname,fname, STRMAX-4);
	strncat(fullname,".lsp", 5);
	fname = fullname;
    }

    /* allocate a file node */
    fptr = cvfile(NULL);

    /* print the information line */
    if (vflag)
	{ sprintf(buf,"; loading \"%s\"\n",fname); stdputstr(buf); }

    /* open the file */
    if ((fp = fopen(fname,"r")) == NULL) {
	xlstack = oldstk;
	return (FALSE);
    }
    setfile(fptr,fp);

    /* read, evaluate and possibly print each expression in the file */
    xlbegin(&cntxt,CF_ERROR,true);
    if (setjmp(cntxt.c_jmpbuf))
	sts = FALSE;
    else {
	while (xlread(fptr,&expr,FALSE)) {
	    expr = xleval(expr);
	    if (pflag)
		stdprint(expr);
	}
	sts = TRUE;
    }
    xlend(&cntxt);

    /* close the file */
    fclose(getfile(fptr));
    setfile(fptr,NULL);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return status */
    return (sts);
}

/* xlread - read an xlisp expression */
int xlread(NODE *fptr,NODE **pval,int rflag)
{
    int sts;

    /* reset the paren nesting level */
    if (!rflag)
	xlplevel = 0;

    /* read an expression */
    while ((sts = readone(fptr,pval)) == FALSE)
	;

    /* return status */
    return (sts == EOF ? FALSE : TRUE);
}

/* readone - attempt to read a single expression */
int readone(NODE *fptr,NODE **pval)
{
    NODE *val,*type;
    int ch;

    /* get a character and check for EOF */
    if ((ch = xlgetc(fptr)) == EOF)
	return (EOF);

    /* handle white space */
    if ((type = tentry(ch)) == k_wspace)
	return (FALSE);

    /* handle symbol constituents */
    else if (type == k_const) {
	*pval = pname(fptr,ch);
	return (TRUE);
    }

    /* handle read macros */
    else if (consp(type)) {
	if ((val = callmacro(fptr,ch)) && consp(val)) {
	    *pval = car(val);
	    return (TRUE);
	}
	else
	    return (FALSE);
    }

    /* handle illegal characters */
    else
	xlerror("illegal character",cvfixnum((FIXNUM)ch));
    /*NOTREACHED*/
}

/* rmhash - read macro for '#' */
NODE *rmhash(NODE *args)
{
    NODE ***oldstk,*fptr __HEAPIFY,*mch __HEAPIFY,*val __HEAPIFY;
    int ch;

    /* create a new stack frame */
    oldstk = xlsave3(&fptr,&mch,&val);

    /* get the file and macro character */
    fptr = xlgetfile(&args);
    mch = xlmatch(INT,&args);
    xllastarg(args);

    /* make the return value */
    val = consa(NIL);

    /* check the next character */
    switch (ch = xlgetc(fptr)) {
    case '\'':
		rplaca(val,pquote(fptr,s_function));
		break;
    case '(':
		rplaca(val,pvector(fptr));
		break;
    case 'x':
    case 'X':
    		rplaca(val,phexnumber(fptr));
		break;
    case '\\':
		rplaca(val,cvfixnum((FIXNUM)xlgetc(fptr)));
		break;
    default:
		xlerror("illegal character after #",cvfixnum((FIXNUM)ch));
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* rmquote - read macro for '\'' */
NODE *rmquote(NODE *args)
{
    NODE ***oldstk,*fptr __HEAPIFY,*mch __HEAPIFY,*val __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave3(&fptr,&mch,&val);

    /* get the file and macro character */
    fptr = xlgetfile(&args);
    mch = xlmatch(INT,&args);
    xllastarg(args);

    /* make the return value */
    val = consa(NIL);
    rplaca(val,pquote(fptr,s_quote));

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* rmdquote - read macro for '"' */
NODE *rmdquote(NODE *args)
{
    NODE ***oldstk,*fptr __HEAPIFY,*mch __HEAPIFY,*val __HEAPIFY;
    int ch,i,d1,d2,d3;

    /* create a new stack frame */
    oldstk = xlsave3(&fptr,&mch,&val);

    /* get the file and macro character */
    fptr = xlgetfile(&args);
    mch = xlmatch(INT,&args);
    xllastarg(args);

    /* loop looking for a closing quote */
    for (i = 0; i < STRMAX && (ch = checkeof(fptr)) != '"'; i++) {
	switch (ch) {
	case '\\':
		switch (ch = checkeof(fptr)) {
		case 'f':
			ch = '\f';
			break;
		case 'n':
			ch = '\n';
			break;
		case 'r':
			ch = '\r';
			break;
		case 't':
			ch = '\t';
			break;
		default:
			if (ch >= '0' && ch <= '7') {
			    d1 = ch - '0';
			    d2 = checkeof(fptr) - '0';
			    d3 = checkeof(fptr) - '0';
			    ch = (d1 << 6) + (d2 << 3) + d3;
			}
			break;
		}
	}
	buf[i] = ch;
    }
    buf[i] = 0;

    /* initialize the node */
    val = consa(NIL);
    rplaca(val,cvstring(buf));

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new string */
    return (val);
}

/* rmbquote - read macro for '`' */
NODE *rmbquote(NODE *args)
{
    NODE ***oldstk,*fptr __HEAPIFY,*mch __HEAPIFY,*val __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave3(&fptr,&mch,&val);

    /* get the file and macro character */
    fptr = xlgetfile(&args);
    mch = xlmatch(INT,&args);
    xllastarg(args);

    /* make the return value */
    val = consa(NIL);
    rplaca(val,pquote(fptr,s_bquote));

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* rmcomma - read macro for ',' */
NODE *rmcomma(NODE *args)
{
    NODE ***oldstk,*fptr __HEAPIFY,*mch __HEAPIFY,*val __HEAPIFY,*sym;

    /* create a new stack frame */
    oldstk = xlsave3(&fptr,&mch,&val);

    /* get the file and macro character */
    fptr = xlgetfile(&args);
    mch = xlmatch(INT,&args);
    xllastarg(args);

    /* check the next character */
    if (xlpeek(fptr) == '@') {
	sym = s_comat;
	xlgetc(fptr);
    }
    else
	sym = s_comma;

    /* make the return value */
    val = consa(NIL);
    rplaca(val,pquote(fptr,sym));

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* rmlpar - read macro for '(' */
NODE *rmlpar(NODE *args)
{
    NODE ***oldstk,*fptr __HEAPIFY,*mch __HEAPIFY,*val __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave3(&fptr,&mch,&val);

    /* get the file and macro character */
    fptr = xlgetfile(&args);
    mch = xlmatch(INT,&args);
    xllastarg(args);

    /* make the return value */
    val = consa(NIL);
    rplaca(val,plist(fptr));

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* rmrpar - read macro for ')' */
NODE *rmrpar(NODE *args)
{
    xlfail("misplaced right paren");
	return NIL; /* The trouble with C is to put functions in a table
			they all have to be the same type MJP */
}

/* rmsemi - read macro for ';' */
NODE *rmsemi(NODE *args)
{
    NODE ***oldstk,*fptr __HEAPIFY,*mch __HEAPIFY;
    int ch;

    /* create a new stack frame */
    oldstk = xlsave2(&fptr,&mch);

    /* get the file and macro character */
    fptr = xlgetfile(&args);
    mch = xlmatch(INT,&args);
    xllastarg(args);

    /* skip to end of line */
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n')
	;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return nil (nothing read) */
    return (NIL);
}

/* phexnumber - parse a hexidecimal number */
LOCAL NODE *phexnumber(NODE *fptr)
{
    long num;
    int ch;
    
    num = 0L;
    while ((ch = xlpeek(fptr)) != EOF) {
	if (islower(ch)) ch = toupper(ch);
	if (!isdigit(ch) && !(ch >= 'A' && ch <= 'F'))
	    break;
	xlgetc(fptr);
	num = num * 16L + (long)(ch <= '9' ? ch - '0' : ch - 'A' + 10);
    }
    return (cvfixnum((FIXNUM)num));
}

/* plist - parse a list */
LOCAL NODE *plist(NODE *fptr)
{
    NODE ***oldstk,*val __HEAPIFY,*expr __HEAPIFY,*lastnptr;
    NODE *nptr = NIL;

    /* create a new stack frame */
    oldstk = xlsave2(&val,&expr);

    /* increase the paren nesting level */
    ++xlplevel;

    /* keep appending nodes until a closing paren is found */
    lastnptr = NIL;
    for (lastnptr = NIL; nextch(fptr) != ')'; lastnptr = nptr)

	/* get the next expression */
	switch (readone(fptr,&expr)) {
	case EOF:
	    badeof(fptr);
	case TRUE:

	    /* check for a dotted tail */
	    if (expr == s_dot) {

		/* make sure there's a node */
		if (lastnptr == NIL)
		    xlfail("invalid dotted pair");

		/* parse the expression after the dot */
		if (!xlread(fptr,&expr,TRUE))
		    badeof(fptr);
		rplacd(lastnptr,expr);

		/* make sure its followed by a close paren */
		if (nextch(fptr) != ')')
		    xlfail("invalid dotted pair");

		/* done with this list */
		break;
	    }

	    /* otherwise, handle a normal list element */
	    else {
		nptr = consa(expr);
		if (lastnptr == NIL)
		    val = nptr;
		else
		    rplacd(lastnptr,nptr);
	    }
	    break;
	}

    /* skip the closing paren */
    xlgetc(fptr);

    /* decrease the paren nesting level */
    --xlplevel;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return successfully */
    return (val);
}

/* pvector - parse a vector */
LOCAL NODE *pvector(NODE *fptr)
{
    NODE ***oldstk,*list __HEAPIFY,*expr __HEAPIFY,*val,*lastnptr;
    NODE *nptr = NIL;
    int len,ch,i;

    /* create a new stack frame */
    oldstk = xlsave2(&list,&expr);

    /* keep appending nodes until a closing paren is found */
    lastnptr = NIL; len = 0;
    for (lastnptr = NIL; (ch = nextch(fptr)) != ')'; lastnptr = nptr) {

	/* check for end of file */
	if (ch == EOF)
	    badeof(fptr);

	/* get the next expression */
	switch (readone(fptr,&expr)) {
	case EOF:
	    badeof(fptr);
	case TRUE:
	    nptr = consa(expr);
	    if (lastnptr == NIL)
		list = nptr;
	    else
		rplacd(lastnptr,nptr);
	    len++;
	    break;
	}
    }

    /* skip the closing paren */
    xlgetc(fptr);

    /* make a vector of the appropriate length */
    val = newvector(len);

    /* copy the list into the vector */
    for (i = 0; i < len; ++i, list = cdr(list))
	setelement(val,i,car(list));

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return successfully */
    return (val);
}

/* pquote - parse a quoted expression */
LOCAL NODE *pquote(NODE *fptr,NODE *sym)
{
    NODE ***oldstk,*val __HEAPIFY,*p;

    /* create a new stack frame */
    oldstk = xlsave1(&val);

    /* allocate two nodes */
    val = consa(sym);
    rplacd(val,consa(NIL));

    /* initialize the second to point to the quoted expression */
    if (!xlread(fptr,&p,TRUE))
	badeof(fptr);
    rplaca(cdr(val),p);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the quoted expression */
    return (val);
}

/* pname - parse a symbol name */
LOCAL NODE *pname(NODE *fptr,int ch)
{
    NODE *val,*type;
    int i;

    /* get symbol name */
    for (i = 0; ; xlgetc(fptr)) {
	if (i < STRMAX)
	    buf[i++] = (islower(ch) ? toupper(ch) : ch);
	if ((ch = xlpeek(fptr)) == EOF ||
	    ((type = tentry(ch)) != k_const &&
             !(consp(type) && car(type) == k_nmacro)))
	    break;
    }
    buf[i] = 0;

    /* check for a number or enter the symbol into the oblist */
    return (isnum(buf,&val) ? val : xlenter(buf,DYNAMIC));
}

/* tentry - get a readtable entry */
LOCAL NODE *tentry(int ch)
{
    NODE *rtable;
    rtable = getvalue(s_rtable);
    if (!vectorp(rtable) || ch < 0 || ch >= getsize(rtable))
	return (NIL);
    return (getelement(rtable,ch));
}

/* nextch - look at the next non-blank character */
LOCAL int nextch(NODE *fptr)
{
    int ch;

    /* return and save the next non-blank character */
    while ((ch = xlpeek(fptr)) != EOF && isspace(ch))
	xlgetc(fptr);
    return (ch);
}

/* checkeof - get a character and check for end of file */
LOCAL int checkeof(NODE *fptr)
{
    int ch;

    if ((ch = xlgetc(fptr)) == EOF)
	badeof(fptr);
    return (ch);
}

/* badeof - unexpected eof */
LOCAL void badeof(NODE *fptr)
{
    xlgetc(fptr);
    xlfail("unexpected EOF");
}

/* isnum - check if this string is a number */
int isnum(char *str,NODE **pval)
{
    int dl,dr;
    char *p;

    /* initialize */
    p = str; dl = dr = 0;

    /* check for a sign */
    if (*p == '+' || *p == '-')
	p++;

    /* check for a string of digits */
    while (isdigit(*p))
	p++, dl++;

    /* check for a decimal point */
    if (*p == '.') {
	p++;
	while (isdigit(*p))
	    p++, dr++;
    }

    /* check for an exponent */
    if ((dl || dr) && *p == 'E') {
	p++;

	/* check for a sign */
	if (*p == '+' || *p == '-')
	    p++;

	/* check for a string of digits */
	while (isdigit(*p))
	    p++, dr++;
    }

    /* make sure there was at least one digit and this is the end */
    if ((dl == 0 && dr == 0) || *p)
	return (FALSE);

    /* convert the string to an integer and return successfully */
    if (*str == '+') ++str;
    if (str[strlen(str)-1] == '.') str[strlen(str)-1] = 0;
    *pval = (dr ? cvflonum(atof(str)) : cvfixnum(ICNV(str)));
    return (TRUE);
}

/* defmacro - define a read macro */
//sm: void defmacro(int ch,NODE *type,NODE *(*fun)())
void defmacro(int ch,NODE *type,NODE *(*fun)(NODE*))
{
    NODE *p;
    p = consa(type);
    setelement(getvalue(s_rtable),ch,p);
    rplacd(p,cvsubr(fun,SUBR));
}

/* callmacro - call a read macro */
NODE *callmacro(NODE *fptr,int ch)
{
    NODE ***oldstk,*fun __HEAPIFY,*args __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave2(&fun,&args);

    /* get the macro function */
    fun = cdr(getelement(getvalue(s_rtable),ch));

    /* create the argument list */
    args = consa(fptr);
    rplacd(args,consa(NIL));
    rplaca(cdr(args),cvfixnum((FIXNUM)ch));

    /* apply the macro function to the arguments */
    val = xlapply(fun,args);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* needsextension - determine if a filename needs an extension */
int needsextension(char *name)
{
    while (*name)
	if (*name++ == '.')
	    return (FALSE);
    return (TRUE);
}

/* xlrinit - initialize the reader */
void xlrinit(void)
{
    NODE *rtable;
    char *p;
    int ch;

    /* create the read table */
    rtable = newvector(256);
    setvalue(s_rtable,rtable);

    /* initialize the readtable */
    for (p = WSPACE; ch = *p; ) { 
      p++; //matth: moved the p++ out of the conditional.
      setelement(rtable,ch,k_wspace);
    }
    for (p = CONST1; ch = *p; ) { 
      p++; 
      setelement(rtable,ch,k_const); 
    }
    for (p = CONST2; ch = *p; ) {
      p++; 
      setelement(rtable,ch,k_const); 
    }

    /* install the read macros */
    defmacro('#', k_nmacro,rmhash);
    defmacro('\'',k_tmacro,rmquote);
    defmacro('"', k_tmacro,rmdquote);
    defmacro('`', k_tmacro,rmbquote);
    defmacro(',', k_tmacro,rmcomma);
    defmacro('(', k_tmacro,rmlpar);
    defmacro(')', k_tmacro,rmrpar);
    defmacro(';', k_tmacro,rmsemi);
}

